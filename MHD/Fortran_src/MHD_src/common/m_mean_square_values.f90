!>@file   m_mean_square_values.f90
!!        module m_mean_square_values
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
      module m_mean_square_values
!
      use m_precision
!
      use t_phys_address
      use t_FEM_MHD_mean_square
!
      implicit  none
!
!
!>      Structure for mean square values
      type(FEM_MHD_mean_square), save :: fem_sq1
!
      end module m_mean_square_values
