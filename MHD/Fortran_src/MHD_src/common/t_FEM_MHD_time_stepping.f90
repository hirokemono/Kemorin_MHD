!>@file   t_FEM_MHD_time_stepping.f90
!!        module t_FEM_MHD_time_stepping
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
      module t_FEM_MHD_time_stepping
!
      use m_precision
!
      use t_flex_delta_t_data
      use t_FEM_MHD_mean_square
!
      implicit  none
!
!
      type FEM_MHD_time_stepping
!>        Data to evaluate flexible step
        type(flexible_stepping_data) :: flex_data
!
!>        Structure for mean square values
        type(FEM_MHD_mean_square) :: fem_sq
      end type FEM_MHD_time_stepping
!
      end module t_FEM_MHD_time_stepping
