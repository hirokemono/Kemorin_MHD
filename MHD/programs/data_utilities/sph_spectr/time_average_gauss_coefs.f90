!>@file   time_average_gauss_coefs.f90
!!@brief  module time_average_gauss_coefs
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of Nusselt number file
!!
!!@verbatim
!!      integer(c_int) function                                         &
!!    &     time_average_nusselt_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine s_time_average_nusselt                               &
!!     &         (file_name, start_time, end_time)
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!@endverbatim
!
      module time_average_gauss_coefs
!
      use ISO_C_BINDING
      use m_precision
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
!
!
! -------------------------------------------------------------------
!
      end module time_average_gauss_coefs
