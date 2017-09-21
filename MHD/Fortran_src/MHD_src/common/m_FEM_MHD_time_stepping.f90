!>@file   m_FEM_MHD_time_stepping.f90
!!        module m_FEM_MHD_time_stepping
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
      module m_FEM_MHD_time_stepping
!
      use m_precision
!
      use t_FEM_MHD_time_stepping
!
      implicit  none
!
!
      type(FEM_MHD_time_stepping), save :: flex_MHD1
!
      end module m_FEM_MHD_time_stepping
