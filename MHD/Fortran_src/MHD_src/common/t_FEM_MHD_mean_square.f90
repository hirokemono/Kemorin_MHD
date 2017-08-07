!>@file   t_FEM_MHD_mean_square.f90
!!        module t_FEM_MHD_mean_square
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
!!@verbatim
!!      subroutine init_FEM_MHD_mean_square(nod_fld, fem_sq)
!!@endverbatim
!
      module t_FEM_MHD_mean_square
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
      use t_mean_square_values
!
      implicit  none
!
!
!>        Structure for mean square data for FEM_MHD
      type FEM_MHD_mean_square
!>        Structure for mean square values
        type(mean_square_values) :: msq
!>        Structure for mean square addresses not listed in phys_address
        type(mean_square_address) :: i_msq
!
!>        Structure for addresses of volume average
        type(phys_address) :: i_rms
!>        Structure for addresses of mean square
        type(phys_address) :: j_ave
      end type FEM_MHD_mean_square
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_FEM_MHD_mean_square(nod_fld, fem_sq)
!
      use set_mean_square_array
!
      type(phys_data), intent(in) :: nod_fld
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
!
      call count_mean_square_values(nod_fld, fem_sq%msq)
      call alloc_mean_square_values(fem_sq%msq)
      call set_mean_square_values                                       &
     &   (nod_fld, fem_sq%i_rms, fem_sq%j_ave, fem_sq%i_msq)
!
      end subroutine init_FEM_MHD_mean_square
!
!-----------------------------------------------------------------------
!
      end module t_FEM_MHD_mean_square
