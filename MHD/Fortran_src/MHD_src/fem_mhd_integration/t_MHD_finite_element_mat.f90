!t_MHD_finite_element_mat.f90
!     module t_MHD_finite_element_mat
!.......................................................................
!
!     Written by H. Matsui on Mar. 2009
!
!>    Structure for lumped mass matrix and RHS vector assembler for MHD
!
!!      subroutine alloc_int_vol_data                                   &
!!     &         (numele, max_nod_smp, SGS_param, nod_fld, mhd_fem_wk)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!      subroutine alloc_int_vol_dvx(numele, mhd_fem_wk)
!!
!!      subroutine dealloc_int_vol_data(nod_fld, mhd_fem_wk)
!!      subroutine dealloc_int_vol_dvx(mhd_fem_wk)
!!
!!      subroutine reset_ff_t_smp(max_nod_smp, mhd_fem_wk)
!!      subroutine reset_ff_m_smp(max_nod_smp, mhd_fem_wk)
!!
!!      subroutine check_ff_m_smp                                       &
!!     &         (id_rank, numdir, max_nod_smp, mhd_fem_wk)
!!      subroutine check_diff_elemental_data                            &
!!     &         (id_rank, numele, numdir, i_field, mhd_fem_wk)
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
!>        Nodal work area for multi-pass
        real (kind=kreal), allocatable  ::  ff_m_smp(:,:,:)
!>        Nodal work area for multi-pass
        real (kind=kreal), allocatable  ::  ff_t_smp(:,:,:)
!
!>        assembled position in each element
        real (kind=kreal), allocatable ::  xx_e(:,:)
!>        assembled radius in each element
        real (kind=kreal), allocatable ::  rr_e(:)
!
!>        assembled velocity in each element
        real (kind=kreal), allocatable ::  velo_1(:,:)
!>        assembled magnetic field in each element
        real (kind=kreal), allocatable ::  magne_1(:,:)
!>        assembled vector potential in each element
        real (kind=kreal), allocatable ::  vecp_1(:,:)
!
!>        assembled SGS vector in each element
        real (kind=kreal), allocatable  :: sgs_v1(:,:)
!>        assembled SGS tensor in each element
        real (kind=kreal), allocatable  :: sgs_t1(:,:)
!
!>        Number of components for work dvx in each element
        integer(kind=kint) :: n_dvx
!>        Work of diffence of field in each element
        real (kind=kreal), allocatable :: dvx(:,:)
      end type work_MHD_fe_mat
!
!   ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_int_vol_data                                     &
     &         (numele, max_nod_smp, SGS_param, nod_fld, mhd_fem_wk)
!
      use m_machine_parameter
      use m_phys_labels
      use t_SGS_control_parameter
      use t_phys_data
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      integer(kind = kint), intent(in) :: numele, max_nod_smp
      type(phys_data), intent(in) :: nod_fld
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: i
!
!
      allocate(mhd_fem_wk%ff_m_smp(max_nod_smp,3,np_smp))
      allocate(mhd_fem_wk%ff_t_smp(max_nod_smp,6,np_smp))
      if(max_nod_smp .gt. 0) mhd_fem_wk%ff_m_smp = 0.0d0
      if(max_nod_smp .gt. 0) mhd_fem_wk%ff_t_smp = 0.0d0
!
      allocate(mhd_fem_wk%xx_e(numele,3))
      allocate(mhd_fem_wk%rr_e(numele))
!
      if(numele .gt. 0) mhd_fem_wk%xx_e = 0.0d0
      if(numele .gt. 0) mhd_fem_wk%rr_e = 0.0d0
!
      do i = 1, nod_fld%num_phys
        if      ( nod_fld%phys_name(i) .eq. fhd_velo ) then
          allocate(mhd_fem_wk%velo_1(numele,3))
          if(numele .gt. 0) mhd_fem_wk%velo_1 = 0.0d0
        else if ( nod_fld%phys_name(i) .eq. fhd_magne ) then
          allocate(mhd_fem_wk%magne_1(numele,3))
          if(numele .gt. 0) mhd_fem_wk%magne_1 = 0.0d0
        else if ( nod_fld%phys_name(i) .eq. fhd_vecp ) then
          allocate(mhd_fem_wk%vecp_1(numele,3))
          if(numele .gt. 0) mhd_fem_wk%vecp_1 = 0.0d0
        end if
      end do
!
      if (SGS_param%iflag_SGS .ne. id_SGS_none) then
        allocate(mhd_fem_wk%sgs_v1(numele,3))
        allocate(mhd_fem_wk%sgs_t1(numele,6))
        if(numele .gt. 0) mhd_fem_wk%sgs_v1 = 0.0d0
        if(numele .gt. 0) mhd_fem_wk%sgs_t1 = 0.0d0
      end if
!
      end subroutine alloc_int_vol_data
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_int_vol_dvx(numele, mhd_fem_wk)
!
      integer(kind = kint), intent(in) :: numele
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      allocate(mhd_fem_wk%dvx(numele,mhd_fem_wk%n_dvx))
      if(numele .gt. 0) mhd_fem_wk%dvx = 0.0d0
!
      end subroutine alloc_int_vol_dvx
!
!  ------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine dealloc_int_vol_data(nod_fld, mhd_fem_wk)
!
      use m_phys_labels
      use t_phys_data
!
      type(phys_data), intent(in) :: nod_fld
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: i
!
!
      deallocate(mhd_fem_wk%ff_m_smp, mhd_fem_wk%ff_t_smp)
      deallocate(mhd_fem_wk%xx_e, mhd_fem_wk%rr_e)
!
      do i = 1, nod_fld%num_phys
        if      ( nod_fld%phys_name(i) .eq. fhd_velo ) then
          deallocate(mhd_fem_wk%velo_1)
        else if ( nod_fld%phys_name(i) .eq. fhd_magne ) then
          deallocate(mhd_fem_wk%magne_1)
        else if ( nod_fld%phys_name(i) .eq. fhd_vecp ) then
          deallocate(mhd_fem_wk%vecp_1)
        end if
      end do
!
      end subroutine dealloc_int_vol_data
!
!   ---------------------------------------------------------------------
!
      subroutine dealloc_int_vol_dvx(mhd_fem_wk)
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      deallocate(mhd_fem_wk%dvx)
!
      end subroutine dealloc_int_vol_dvx
!
!  ------------------------------------------------------------------
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
      subroutine check_ff_m_smp                                         &
     &         (id_rank, numdir, max_nod_smp, mhd_fem_wk)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numdir, max_nod_smp
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: ip, inod, nd
!
      write(50+id_rank,*) 'ip, inod, ff_m_smp', numdir
      do ip = 1, np_smp
       do inod = 1, max_nod_smp
         write(50+id_rank,'(2i16,1p10e25.14)')                          &
     &       ip, inod, (mhd_fem_wk%ff_m_smp(inod,nd,ip),nd=1, numdir)
        end do
      end do
!
      end subroutine check_ff_m_smp
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine check_diff_elemental_data                              &
     &         (id_rank, numele, numdir, i_field, mhd_fem_wk)
!
      integer(kind = kint), intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numele, numdir, i_field
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      integer(kind = kint) :: iele, nd, ndiff
!
      do nd = 1, numdir
        write(50+id_rank,*)                                             &
     &      'iele, diff. of elemental field: ', i_field, nd
       do iele = 1, numele
        write(50+id_rank,'(i16,1p10e25.14)') iele,                      &
     &    (mhd_fem_wk%dvx(iele,i_field+3*(nd-1)+ndiff-1),ndiff=1, 3)
       end do
      end do
!
      end subroutine check_diff_elemental_data
!
!-----------------------------------------------------------------------
!
      end module t_MHD_finite_element_mat
