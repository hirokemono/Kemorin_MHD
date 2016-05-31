!>@file   m_sph_spectr_data.f90
!!@brief  module m_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Flag and parameters for spherical transform dnyamo model
!!
!!
!!@verbatim
!!      subroutine init_reference_fields(sph_params, sph_rj, sph_bc_T)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_boundary_type), intent(inout) :: sph_bc_T
!!      subroutine deallocate_reft_rj_data
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module m_sph_spectr_data
!
      use m_precision
      use t_phys_data
      use t_spheric_parameter
      use t_spheric_rj_data
!
      implicit  none
!
!>        Structure for field data
        type(phys_data), save :: rj_fld1
!
!
!>      Number of radial points for reference temperature
      integer(kind = kint) :: nri_reftemp
!
!>    reference temerature spectr @f$ f(r,j) @f$
!!@verbatim
!!        reftemp_rj(kr,0) ... T_0
!!        reftemp_rj(kr,1) ... d T_0 / dr
!!@endverbatim
      real (kind=kreal), allocatable :: reftemp_rj(:,:)
!
      private :: allocate_reft_rj_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_reft_rj_data(sph_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      nri_reftemp = sph_rj%nidx_rj(1)
      allocate( reftemp_rj(nri_reftemp,0:1)   )
      reftemp_rj =  0.0d0
!
      end subroutine allocate_reft_rj_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_reference_fields(sph_params, sph_rj, sph_bc_T)
!
      use t_boundary_params_sph_MHD
      use m_machine_parameter
      use set_reference_sph_mhd
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(sph_boundary_type), intent(inout) :: sph_bc_T
!
!      Set reference temperature and adjust boundary conditions
!
      if(iflag_debug .gt. 0) write(*,*) 'set_ref_temp_sph_mhd'
      call allocate_reft_rj_data(sph_rj)
      call set_ref_temp_sph_mhd(sph_rj%nidx_rj,                         &
     &    sph_params%radius_ICB, sph_params%radius_CMB,                 &
     &    sph_rj%ar_1d_rj, sph_bc_T, reftemp_rj)
      call adjust_sph_temp_bc_by_reftemp                                &
     &   (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),                 &
     &    reftemp_rj, sph_bc_T)
!
      end subroutine init_reference_fields
!
! -----------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine deallocate_reft_rj_data
!
      deallocate( reftemp_rj )
!
      end subroutine deallocate_reft_rj_data
!
!  --------------------------------------------------------------------
!
      end module m_sph_spectr_data
