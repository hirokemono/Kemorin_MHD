!>@file   t_mean_square_values.f90
!!        module t_mean_square_values
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
!!@verbatim
!!      subroutine alloc_mean_square_values(fem_msq)
!!      subroutine dealloc_mean_square_values(fem_msq)
!!        type(mean_square_values), intent(inout) :: fem_msq
!!@endverbatim
!
      module t_mean_square_values
!
      use m_precision
      use t_phys_address
!
      implicit  none
!
!>      Structure for mean square values
      type mean_square_values
!>        number of fields for volume average data
        integer (kind = kint) :: num_ave
!>        number of fields for volume mean square data
        integer (kind = kint) :: num_rms
!
!>        volume average data for each subdomaine
        real(kind=kreal), allocatable :: ave_local(:)
!>        volume average data for entire domain
        real(kind=kreal), allocatable :: ave_global(:)
!
!>        volume mean square data for each subdomaine
        real(kind=kreal), allocatable :: rms_local(:)
!>        volume mean square data for entire domain
        real(kind=kreal), allocatable :: rms_global(:)
!
!>        Structure for addresses of volume average
        type(phys_address) :: i_rms
!>        Structure for addresses of mean square
        type(phys_address) :: j_ave
!
!
!>        Address for root mean square of vorticity
        integer(kind=kint) :: ir_rms_w = 0
!
!>        Address for average of angular momentum
        integer(kind=kint) :: ja_amom = 0
!
!>        Address for magnetic energy including inner core
        integer(kind=kint) :: ir_me_ic = 0
!>        Address for average magnetic field including inner core
        integer(kind=kint) :: ja_mag_ic = 0
!
!
!>        Address for mean square of current density including inner core
        integer(kind=kint) :: ir_sqj_ic = 0
!>        Address for average of current density including inner core
        integer(kind=kint) :: ja_j_ic = 0
!
!>        Address for RMS of current density
        integer(kind=kint) :: ir_rms_j = 0
!>        Address for RMS of current density including inner core
        integer(kind=kint) :: ir_rms_j_ic = 0
!
!>        Address for average of filtered angular momentum
        integer(kind=kint) :: jr_amom_f = 0
!
!>        Address for filtered magnetic energy including inner core
        integer(kind=kint) :: ir_me_f_ic = 0
!>        Address for average filtererd magnetic field
!!        including inner core
        integer(kind=kint) :: ja_mag_f_ic = 0
!
!>        Address of volume of fluid area
        integer(kind=kint) :: ivol = 0
      end type mean_square_values
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_mean_square_values(fem_msq)
!
      type(mean_square_values), intent(inout) :: fem_msq
!
!
      allocate(fem_msq%rms_local(fem_msq%num_rms))
      allocate(fem_msq%rms_global(fem_msq%num_rms))
      allocate(fem_msq%ave_local(fem_msq%num_ave))
      allocate(fem_msq%ave_global(fem_msq%num_ave))
!
      if(fem_msq%num_rms .gt. 0) then
        fem_msq%rms_local  = 0.0d0
        fem_msq%rms_global = 0.0d0
      end if
      if(fem_msq%num_ave .gt. 0) then
        fem_msq%ave_local  = 0.0d0
        fem_msq%ave_global = 0.0d0
      end if
!
      end subroutine alloc_mean_square_values
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mean_square_values(fem_msq)
!
      type(mean_square_values), intent(inout) :: fem_msq
!
!
      deallocate(fem_msq%rms_local)
      deallocate(fem_msq%rms_global)
      deallocate(fem_msq%ave_local)
      deallocate(fem_msq%ave_global)
!
      end subroutine dealloc_mean_square_values
!
! ----------------------------------------------------------------------
!
      end module t_mean_square_values
