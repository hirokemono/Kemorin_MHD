!>@file   m_SPH_MHD_model_data.f90
!!@brief  module m_SPH_MHD_model_data
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
      module m_SPH_MHD_model_data
!
      use m_precision
      use t_SPH_MHD_model_data
      use t_boundary_data_sph_MHD
!
      implicit  none
!
!
!>      Parameters for spectr dynamo model
      type(SPH_MHD_model_data), save :: SPH_model1
!
!>      Boudary conditions for spectr dynamo model
      type(sph_MHD_boundary_data), save :: sph_MHD_bc1
!
      end module m_SPH_MHD_model_data
