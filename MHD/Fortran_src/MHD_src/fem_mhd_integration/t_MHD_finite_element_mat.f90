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
!!      subroutine reset_ff_t_smp(max_nod_smp, mhd_fem_wk)
!!      subroutine reset_ff_m_smp(max_nod_smp, mhd_fem_wk)
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
      use t_finite_element_mat
!
      implicit  none
!
!>      Work array for FEM assemble in MHD model
      type work_MHD_fe_mat
!>      Lumped mass matrix for fluid
        type(lumped_mass_matrices) :: mlump_fl
!>        Lumped mass matrix for counductor
        type(lumped_mass_matrices) :: mlump_cd
!>        Lumped mass matrix for insulator
        type(lumped_mass_matrices) :: mlump_ins
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
      call alloc_type_fem_lumped_mass(numnod, mhd_fem_wk%mlump_fl)
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
      call alloc_type_fem_lumped_mass(numnod, mhd_fem_wk%mlump_cd)
      call alloc_type_fem_lumped_mass(numnod, mhd_fem_wk%mlump_ins)
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
      call dealloc_type_fem_lumped_mass(mhd_fem_wk%mlump_fl)
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
      call dealloc_type_fem_lumped_mass(mhd_fem_wk%mlump_cd)
      call dealloc_type_fem_lumped_mass(mhd_fem_wk%mlump_ins)
!
      end subroutine dealloc_mass_mat_conduct
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_t_smp(max_nod_smp, mhd_fem_wk)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: max_nod_smp
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, np_smp
        mhd_fem_wk%ff_t_smp(1:max_nod_smp,1,ip) =   0.0d0
        mhd_fem_wk%ff_t_smp(1:max_nod_smp,2,ip) =   0.0d0
        mhd_fem_wk%ff_t_smp(1:max_nod_smp,3,ip) =   0.0d0
        mhd_fem_wk%ff_t_smp(1:max_nod_smp,4,ip) =   0.0d0
        mhd_fem_wk%ff_t_smp(1:max_nod_smp,5,ip) =   0.0d0
        mhd_fem_wk%ff_t_smp(1:max_nod_smp,6,ip) =   0.0d0
      end do
!$omp end parallel do
!
      end subroutine reset_ff_t_smp
!
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_m_smp(max_nod_smp, mhd_fem_wk)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: max_nod_smp
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, np_smp
        mhd_fem_wk%ff_m_smp(1:max_nod_smp,1,ip) =   0.0d0
        mhd_fem_wk%ff_m_smp(1:max_nod_smp,2,ip) =   0.0d0
        mhd_fem_wk%ff_m_smp(1:max_nod_smp,3,ip) =   0.0d0
      end do
!$omp end parallel do
!
      end subroutine reset_ff_m_smp
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
!
      write(50+my_rank,*) 'Check ml_fl'
      call check_mass_martix(my_rank, numnod, mhd_fem_wk%mlump_fl)
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
!
      write(50+my_rank,*) 'Check ml_ins'
      call check_mass_martix(my_rank, numnod, mhd_fem_wk%mlump_cd)
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
!
      write(50+my_rank,*) 'Check ml_ins'
      call check_mass_martix(my_rank, numnod, mhd_fem_wk%mlump_ins)
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
