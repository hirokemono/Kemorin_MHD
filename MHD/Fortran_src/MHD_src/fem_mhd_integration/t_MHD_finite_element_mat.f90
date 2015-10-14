!t_MHD_finite_element_mat.f90
!     module t_MHD_finite_element_mat
!.......................................................................
!
!     Written by H. Matsui on Mar. 2009
!
!>    Structure for lumped mass matrix and RHS vector assembler for MHD
!
!!      subroutine alloc_mass_mat_fluid(numnod, mhd_fem_wk)
!!      subroutine alloc_mass_mat_conduct(mhd_fem_wk)
!!      subroutine dealloc_mass_mat_fluid(mhd_fem_wk)
!!      subroutine dealloc_mass_mat_conduct(mhd_fem_wk)
!!
!!      subroutine check_mass_martix_conduct                            &
!!     &         (my_rank, numnod, mhd_fem_wk)
!!      subroutine check_mass_martix_insulate                           &
!!     &         (my_rank, numnod, mhd_fem_wk)
!!      subroutine check_ff_m_smp                                       &
!!     &         (my_rank, numdir, max_nod_smp, mhd_fem_wk)
!
      module t_MHD_finite_element_mat
!
      use m_precision
!
      implicit  none
!
!>      Work array for FEM assemble in MHD model
      type work_MHD_fe_mat
!>      Lumped mass matrix for fluid
        real (kind=kreal), pointer  ::  ml_o_fl(:)
!>      1 / ml_o_fl
        real (kind=kreal), pointer  ::  ml_fl(:)
!
!>      Lumped mass matrix for counductor
        real (kind=kreal), pointer  ::  ml_o_cd(:)
!>      1 / ml_o_cd
        real (kind=kreal), pointer  ::  ml_cd(:)
!
!>      Lumped mass matrix for insulator
        real (kind=kreal), pointer  ::  ml_o_ins(:)
!>      1 / ml_o_ins
        real (kind=kreal), pointer  ::  ml_ins(:)
!
!>        Nodal work area for multi-pass
        real (kind=kreal), pointer  ::  ff_m_smp(:,:,:)
!>        Nodal work area for multi-pass
        real (kind=kreal), pointer  ::  ff_t_smp(:,:,:)
!
!>        assembled position in each element
        real (kind=kreal), pointer ::  xx_e(:,:)
!>        assembled radius in each element
        real (kind=kreal), pointer ::  rr_e(:)
!
!>        assembled velocity in each element
        real (kind=kreal), pointer ::  velo_1(:,:)
!>        assembled magnetic field in each element
        real (kind=kreal), pointer ::  magne_1(:,:)
!>        assembled vector potential in each element
        real (kind=kreal), pointer ::  vecp_1(:,:)
!
!>        assembled SGS vector in each element
        real (kind=kreal), pointer  :: sgs_v1(:,:)
!>        assembled SGS tensor in each element
        real (kind=kreal), pointer  :: sgs_t1(:,:)
!
!>        Number of components for work dvx in each element
        integer(kind=kint) :: n_dvx
!>        Work of diffence of field in each element
        real (kind=kreal), pointer :: dvx(:,:)
      end type work_MHD_fe_mat
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_mass_mat_fluid(numnod, mhd_fem_wk)
!
      integer(kind = kint), intent(in) :: numnod
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      allocate(mhd_fem_wk%ml_fl(numnod))
      allocate(mhd_fem_wk%ml_o_fl(numnod))
!
      if(numnod .le. 0) return
      mhd_fem_wk%ml_fl =   0.0d0
      mhd_fem_wk%ml_o_fl = 0.0d0
!
      end subroutine alloc_mass_mat_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_mass_mat_conduct(numnod, mhd_fem_wk)
!
      integer(kind = kint), intent(in) :: numnod
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      allocate(mhd_fem_wk%ml_cd(numnod))
      allocate(mhd_fem_wk%ml_ins(numnod))
!
      allocate(mhd_fem_wk%ml_o_cd(numnod))
      allocate(mhd_fem_wk%ml_o_ins(numnod))
!
      if(numnod .le. 0) return
      mhd_fem_wk%ml_cd =  0.0d0
      mhd_fem_wk%ml_ins = 0.0d0
!
      mhd_fem_wk%ml_o_cd =  0.0d0
      mhd_fem_wk%ml_o_ins = 0.0d0
!
      end subroutine alloc_mass_mat_conduct
!
!   ---------------------------------------------------------------------
!
      subroutine dealloc_mass_mat_fluid(mhd_fem_wk)
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      deallocate(mhd_fem_wk%ml_fl, mhd_fem_wk%ml_o_fl)
!
      end subroutine dealloc_mass_mat_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine dealloc_mass_mat_conduct(mhd_fem_wk)
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      deallocate(mhd_fem_wk%ml_cd, mhd_fem_wk%ml_ins)
      deallocate(mhd_fem_wk%ml_o_cd, mhd_fem_wk%ml_o_ins)
!
      end subroutine dealloc_mass_mat_conduct
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_fluid                                &
     &         (my_rank, numnod, mhd_fem_wk)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: inod
!
      write(50+my_rank,*) 'inod, ml_fl, ml_o_fl'
      do inod = 1, numnod
        write(50+my_rank,'(i16,1p2e25.14)')                             &
     &           inod, mhd_fem_wk%ml_fl(inod), mhd_fem_wk%ml_o_fl(inod)
      end do
!
      end subroutine check_mass_martix_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_conduct                              &
     &         (my_rank, numnod, mhd_fem_wk)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: inod
!
      write(50+my_rank,*) 'inod, ml_cd, ml_o_cd'
      do inod = 1, numnod
        write(50+my_rank,'(i16,1p2e25.14)')                             &
     &       inod, mhd_fem_wk%ml_cd(inod), mhd_fem_wk%ml_o_cd(inod)
      end do
!
      end subroutine check_mass_martix_conduct
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_insulate                             &
     &         (my_rank, numnod, mhd_fem_wk)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: inod
!
      write(50+my_rank,*) 'inod, ml_ins, ml_o_ins'
      do inod = 1, numnod
        write(50+my_rank,'(i16,1p2e25.14)')                             &
     &        inod, mhd_fem_wk%ml_ins(inod), mhd_fem_wk%ml_o_ins(inod)
      end do
!
      end subroutine check_mass_martix_insulate
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff_m_smp                                         &
     &         (my_rank, numdir, max_nod_smp, mhd_fem_wk)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: my_rank, numdir, max_nod_smp
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: ip, inod, nd
!
      write(50+my_rank,*) 'ip, inod, ff_m_smp', numdir
      do ip = 1, np_smp
       do inod = 1, max_nod_smp
         write(50+my_rank,'(2i16,1p10e25.14)')                          &
     &       ip, inod, (mhd_fem_wk%ff_m_smp(inod,nd,ip),nd=1, numdir)
        end do
      end do
!
      end subroutine check_ff_m_smp
!
!   ---------------------------------------------------------------------
!
      end module t_MHD_finite_element_mat
