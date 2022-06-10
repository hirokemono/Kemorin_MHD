!>@file   time_average_sph_ene_spectr.f90
!!@brief  module time_average_sph_ene_spectr
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of spectrum data
!!
!!@verbatim
!!      integer(c_int) function                                         &
!!    &     time_ave_sdev_sph_volume_pwr_c(cname, cstart, cend) Bind(C)
!!      integer(c_int) function                                         &
!!    &     time_ave_sph_vol_spectr_sdev_c(cname, cstart, cend) Bind(C)
!!      integer(c_int) function                                         &
!!    &     time_ave_sph_layer_pwr_sdev_c(cname, cstart, cend) Bind(C)
!!      integer(c_int) function                                         &
!!    &     time_ave_sph_layer_spec_sdev_c(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine time_ave_sdev_sph_volume_pwr_f                       &
!!     &         (file_prefix, start_time, end_time)
!!      subroutine time_ave_sph_vol_spectr_sdev_f                       &
!!     &         (file_prefix, start_time, end_time)
!!      subroutine time_ave_sph_layer_pwr_sdev_f                        &
!!     &         (file_prefix, start_time, end_time)
!!      subroutine time_ave_sph_layer_spec_sdev_f                       &
!!     &        (file_prefix, start_time, end_time)
!!        character(len=kchara), intent(in) :: file_prefix
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!@endverbatim
!
      module time_average_sph_ene_spectr
!
      use m_precision
      use m_constants
!
      implicit  none
!
      logical, parameter, private :: flag_vol_ave_ON =  .TRUE.
      logical, parameter, private :: flag_vol_ave_OFF = .FALSE.
      logical, parameter, private :: flag_spectr_ON =   .TRUE.
      logical, parameter, private :: flag_spectr_OFF =  .FALSE.
!
      private :: c_to_fstring
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      function c_to_fstring(string)
!
      use Iso_C_binding
!
      Character(1,C_char),Intent(In) :: string(*)
      Character(:,C_char),Allocatable :: c_to_fstring
!
      Integer i,len
      len = 1
      Do While (string(len)/=C_null_char)
        len = len + 1
      End Do
      len = len - 1
      Allocate(Character(len,C_char) :: c_to_fstring)
      Do i=1,len
        c_to_fstring(i:i) = string(i)
      End Do
!
      end function c_to_fstring
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_sdev_sph_volume_pwr_c(cname, cstart, cend) Bind(C)
!
      use Iso_C_binding
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: file_prefix
!
      write(file_prefix,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call time_ave_sdev_sph_volume_pwr_f                               &
     &   (file_prefix, start_time, end_time)
!
      time_ave_sdev_sph_volume_pwr_c = 0
      end function time_ave_sdev_sph_volume_pwr_c
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_sph_vol_spectr_sdev_c(cname, cstart, cend) Bind(C)
!
      use Iso_C_binding
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: file_prefix
!
      write(file_prefix,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call time_ave_sph_vol_spectr_sdev_f                               &
     &   (file_prefix, start_time, end_time)
!
      time_ave_sph_vol_spectr_sdev_c = 0
      end function time_ave_sph_vol_spectr_sdev_c
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_sph_layer_pwr_sdev_c(cname, cstart, cend) Bind(C)
!
      use Iso_C_binding
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: file_prefix
!
      write(file_prefix,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call time_ave_sph_layer_pwr_sdev_f                                &
     &   (file_prefix, start_time, end_time)
!
      time_ave_sph_layer_pwr_sdev_c = 0
      end function time_ave_sph_layer_pwr_sdev_c
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_sph_layer_spec_sdev_c(cname, cstart, cend) Bind(C)
!
      use Iso_C_binding
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: file_prefix
!
      write(file_prefix,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call time_ave_sph_layer_spec_sdev_f                               &
     &   (file_prefix, start_time, end_time)
!
      time_ave_sph_layer_spec_sdev_c = 0
      end function time_ave_sph_layer_spec_sdev_c
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine time_ave_sdev_sph_volume_pwr_f                         &
     &         (file_prefix, start_time, end_time)
!
      use m_tave_sph_ene_spectr
!
      character(len = kchara), intent(in) :: file_prefix
      real(kind = kreal), intent(in) :: start_time, end_time
!
      call time_ave_sdev_sph_spectr                                     &
     &   (file_prefix, flag_spectr_OFF, flag_vol_ave_ON,                &
     &    start_time, end_time)
!
      end subroutine time_ave_sdev_sph_volume_pwr_f
!
! -------------------------------------------------------------------
!
      subroutine time_ave_sph_vol_spectr_sdev_f                         &
     &         (file_prefix, start_time, end_time)
!
      use m_tave_sph_ene_spectr
!
      character(len = kchara), intent(in) :: file_prefix
      real(kind = kreal), intent(in) :: start_time, end_time
!
      call time_ave_sdev_sph_spectr                                     &
     &   (file_prefix, flag_spectr_ON, flag_vol_ave_ON,                 &
     &    start_time, end_time)
!
      end subroutine time_ave_sph_vol_spectr_sdev_f
!
! -------------------------------------------------------------------
!
      subroutine time_ave_sph_layer_pwr_sdev_f                          &
     &         (file_prefix, start_time, end_time)
!
      use m_tave_sph_ene_spectr
!
      character(len = kchara), intent(in) :: file_prefix
      real(kind = kreal), intent(in) :: start_time, end_time
!
      call time_ave_sdev_sph_spectr                                     &
     &   (file_prefix, flag_spectr_OFF, flag_vol_ave_Off,               &
     &    start_time, end_time)
!
      end subroutine time_ave_sph_layer_pwr_sdev_f
!
! -------------------------------------------------------------------
!
      subroutine time_ave_sph_layer_spec_sdev_f                         &
     &        (file_prefix, start_time, end_time)
!
      use m_tave_sph_ene_spectr
!
      character(len = kchara), intent(in) :: file_prefix
      real(kind = kreal), intent(in) :: start_time, end_time
!
      call time_ave_sdev_sph_spectr                                     &
     &   (file_prefix, flag_spectr_ON, flag_vol_ave_OFF,                &
     &    start_time, end_time)
!
      end subroutine time_ave_sph_layer_spec_sdev_f
!
! -------------------------------------------------------------------
!
      end module time_average_sph_ene_spectr
