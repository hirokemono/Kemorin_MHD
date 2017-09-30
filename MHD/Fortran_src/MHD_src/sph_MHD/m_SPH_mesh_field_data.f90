!>@file   m_SPH_mesh_field_data.f90
!!@brief  module m_SPH_mesh_field_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Flag and parameters for spherical transform dnyamo model
!!
!!
!!@verbatim
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module m_SPH_mesh_field_data
!
      use m_precision
!
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_work_SPH_MHD
!
      implicit  none
!
!
!>      Structure of spetr grid and data
      type(SPH_mesh_field_data), save :: SPH_MHD1
!
!>        Structures of work area for spherical shell dynamo
      type(work_SPH_MHD), save :: SPH_WK1
!
      end module m_SPH_mesh_field_data
