!>@file   time_average_dipolarity.f90
!!@brief  module time_average_dipolarity
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of dipolarity file
!!
!!@verbatim
!!      integer(c_int) function                                         &
!!    &     time_average_dipolarity_c(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine time_average_dipolarity_f                            &
!!     &         (file_prefix, start_time, end_time)
!!        character(len=kchara), intent(in) :: file_prefix
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!@endverbatim
!
      module time_average_dipolarity
!
      implicit  none
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
!
      integer(c_int) function                                           &
    &     time_average_dipolarity_c(cname, cstart, cend) Bind(C)
!
      use Iso_C_binding
      use m_precision
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
      call time_average_dipolarity_f(file_prefix, start_time, end_time)
!
      time_average_dipolarity_c = 0
      end function time_average_dipolarity_c
!
! -------------------------------------------------------------------
!
      subroutine time_average_dipolarity_f                              &
     &         (file_prefix, start_time, end_time)
!
      use m_precision
      use m_constants
!
      use t_CMB_dipolarity
      use set_parallel_file_name
!
      implicit  none
!
      character(len=kchara), intent(in) :: file_prefix
      real(kind = kreal), intent(in) :: start_time, end_time

      type(dipolarity_data), save :: dip_t
!
      real(kind = kreal) :: prev_fdip = 0.0d0
      real(kind = kreal) :: ave_fdip, sdev_fdip
      integer(kind = kint), parameter :: id_dipolarity = 15
!
      integer(kind = kint) :: i_step, icou, i
      real(kind = kreal) :: acou, time, prev_time, radius_CMB
      real(kind = kreal) :: true_start
      character(len = kchara) :: file_name, tmpchara
!
!
      dip_t%dipolarity_prefix = file_prefix
      file_name = add_dat_extension(dip_t%dipolarity_prefix)
!
      open(id_dipolarity, file = file_name,                             &
     &     form='formatted', status='old')
      read(id_dipolarity,*)  tmpchara
      read(id_dipolarity,*)  dip_t%ltr_max, radius_CMB
      read(id_dipolarity,*)  tmpchara
!
!       Evaluate time average
!
      icou = 0
      time = start_time
      prev_time = start_time
      ave_fdip = 0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' averaging finished. Count=  ', icou
      do
        read(id_dipolarity,*,err=99) i_step, time, dip_t%f_dip
        if(time .ge. start_time) then
!
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_fdip = dip_t%f_dip
          else
            ave_fdip = ave_fdip + half*(dip_t%f_dip + prev_fdip)        &
     &                 * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_fdip = dip_t%f_dip
!
!        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59),&
!     &       'step= ', i_step,  ' averaging finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
  99  continue
      write(*,*)
      close(id_dipolarity)
!
      acou = one / (time - true_start)
      ave_fdip = ave_fdip * acou
!
!       Evaluate standard deviation
!
      open(id_dipolarity, file = file_name,                             &
     &     form='formatted', status='old')
      read(id_dipolarity,*)  tmpchara
      read(id_dipolarity,*)  dip_t%ltr_max, radius_CMB
      read(id_dipolarity,*)  tmpchara
!
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' deviation finished. Count=  ', icou
      icou = 0
      time = start_time
      prev_time = start_time
      sdev_fdip = 0.0d0
      do
        read(id_dipolarity,*,err=98) i_step, time, dip_t%f_dip
!
        if(time .ge. start_time) then
!
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_fdip = dip_t%f_dip
          else
            sdev_fdip = sdev_fdip                                       &
     &           + half*( (dip_t%f_dip - ave_fdip)**2 + prev_fdip)      &
     &                   * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_fdip = (dip_t%f_dip - ave_fdip)**2
!
!        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59),&
!     &       'step= ', i_step,  ' deviation finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
  98  continue
      write(*,*)
      close(id_dipolarity)
!
      acou = one / (time - true_start)
      sdev_fdip = sqrt(sdev_fdip * acou)
!
!    output Results
!
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, end_time
      write(*,'(a)') 'Average and Std. Dev. of Nu at CMB:'
      write(*,'(1p2e25.15e3)')  ave_fdip, sdev_fdip
!
      end subroutine time_average_dipolarity_f
!
! -------------------------------------------------------------------
!
      end module time_average_dipolarity
