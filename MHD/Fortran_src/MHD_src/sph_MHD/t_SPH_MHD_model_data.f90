!>@file   t_SPH_MHD_model_data.f90
!!@brief  module t_SPH_MHD_model_data
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
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
!!*        ref_temp%t_rj(kr,0) ... T_0
!!*        ref_temp%t_rj(kr,1) ... d T_0 / dr
!!*
!!***********************************************************************
!!@endverbatim
!
      module t_SPH_MHD_model_data
!
      use m_precision
      use t_control_parameter
      use t_poloidal_rotation
      use t_radial_reference_temp
!
      implicit  none
!
!
!
!>      Parameters for spectr dynamo model
      type SPH_MHD_model_data
        type(MHD_evolution_param) :: MHD_prop
!
!>        Structure for rotatin vector
        type(sph_rotation) :: omega_sph
!
!>        Structure of reference temperature
        type(reference_temperature) :: ref_temp
!>        Structure of reference temperature
        type(reference_temperature) :: ref_comp
      end type SPH_MHD_model_data
!
      end module t_SPH_MHD_model_data
