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
!!
!!***********************************************************************
!!*
!!*     rot_e(k,j) : rotation of earth  (output)
!!*     rot_e(k,j) : d \Omega / dr
!!*     rot_e(k,j) : d^2 \Omega / dr^2
!!*
!!*                       1
!!*         rot_e(k,j) = --- r^2
!!*                       2
!!*
!!*                     dom(k,0)
!!*       drot_e(k,j) = ---------
!!*                        dr
!!*                   = r(k)
!!*
!!*                      dom(k,0)
!!*       d2rot_e(k,j) = ---------
!!*                         dr
!!*                    = 1.0
!!*
!!***********************************************************************
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module m_sph_spectr_data
!
      use m_precision
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_poloidal_rotation
      use t_phys_data
!
      implicit  none
!
!>      Structure for field data
      type(phys_data), save :: rj_fld1
!
!>      Structure for rotatin vector
      type(sph_rotation), save :: omega_sph1
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
