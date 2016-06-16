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
!!*        ref_temp1%t_rj(kr,0) ... T_0
!!*        ref_temp1%t_rj(kr,1) ... d T_0 / dr
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
      use t_radial_reference_temp
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
!>      Structure of reference temperature
      type(reference_temperature), save :: ref_temp1
!ref_temp1%t_rj
!
      end module m_sph_spectr_data
