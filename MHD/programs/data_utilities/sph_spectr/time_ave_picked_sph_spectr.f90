!>@file   time_ave_picked_sph_spectr.f90
!!@brief  module time_ave_picked_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of spectrum data
!!
!!@verbatim
!!        integer(c_int) function                                       &
!!      &     time_ave_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!      subroutine s_time_ave_picked_sph_spectr                         &
!!     &         (file_name, start_time, end_time)
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!@endverbatim
!
      module time_ave_picked_sph_spectr
!
      use Iso_C_binding
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
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
    &     time_ave_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: file_name
!
      write(file_name,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call s_time_ave_picked_sph_spectr                                 &
     &   (file_name, start_time, end_time)
!
      time_ave_picked_sph_spectr_f = 0
      end function time_ave_picked_sph_spectr_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine s_time_ave_picked_sph_spectr                           &
     &         (file_name, start_time, end_time)
!
      use m_precision
      use m_constants
!
      use t_picked_sph_spectr_data_IO
      use t_ctl_data_tave_sph_monitor
      use set_parallel_file_name
!
      implicit  none
!
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time
!
!>      Structure for control data
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
!
      type(picked_spectrum_data_IO), save :: pick_IO
!
      real(kind = kreal), allocatable :: prev_spec(:,:)
      real(kind = kreal), allocatable :: ave_spec(:,:)
      real(kind = kreal), allocatable :: rms_spec(:,:)
      real(kind = kreal), allocatable :: sdev_spec(:,:)
!
      character(len=kchara) :: tave_fname
      character(len=kchara) :: trms_fname
      character(len=kchara) :: sdev_fname
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: i_step, ierr, icou, i, nd
      real(kind = kreal) :: acou, time, prev_time
      real(kind = kreal) :: true_start, true_end
!
!
      write(tave_fname,'(a6,a)') 't_ave_', trim(file_name)
      write(trms_fname,'(a8,a)') 't_rms_', trim(file_name)
      write(sdev_fname,'(a8,a)') 't_sigma_', trim(file_name)
!
!      Open picked mode file
!
      call open_sph_spec_read(id_pick, file_name, pick_IO)
!
      allocate(prev_spec(pick_IO%ntot_comp,pick_IO%ntot_pick_spectr))
      allocate(ave_spec(pick_IO%ntot_comp,pick_IO%ntot_pick_spectr))
      allocate(rms_spec(pick_IO%ntot_comp,pick_IO%ntot_pick_spectr))
      allocate(sdev_spec(pick_IO%ntot_comp,pick_IO%ntot_pick_spectr))
!
!$omp parallel workshare
      prev_spec =  0.0d0
      ave_spec =   0.0d0
      rms_spec =   0.0d0
      sdev_spec =  0.0d0
!$omp end parallel workshare
!
!       Evaluate time average
!
      icou = 0
      true_start = start_time
      prev_time = true_start
      do
        call read_sph_spec_monitor                                      &
     &     (id_pick, i_step, time, pick_IO, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          if(icou .eq. 0) then
            true_start = time
          else
!
!$omp parallel
            do i = 1, pick_IO%ntot_pick_spectr
!$omp do
              do nd = 1, pick_IO%ntot_comp
                ave_spec(nd,i) = ave_spec(nd,i) + half                  &
     &           * (pick_IO%d_pk(nd,i) + prev_spec(nd,i))               &
     &           * (time - prev_time)
                rms_spec(nd,i) = rms_spec(nd,i) + half                  &
     &           * (pick_IO%d_pk(nd,i)**2 + prev_spec(nd,i)**2)         &
     &           * (time - prev_time)
              end do
!$omp end do nowait
            end do
!$omp end parallel
          end if
!
!$omp parallel
          do i = 1, pick_IO%ntot_pick_spectr
!$omp do
            do nd = 1, pick_IO%ntot_comp
              prev_spec(nd,i) = pick_IO%d_pk(nd,i)
            end do
!$omp end do nowait
          end do
!$omp end parallel
!
          icou = icou + 1
          write(*,*) 'step ', i_step,                                   &
     &        ' is added for time average: count is  ', icou, time
        end if
        prev_time = time
!
        if(time .ge. end_time) exit
      end do
      close(id_pick)
!
      acou = one / (time - true_start)
!$omp parallel
      do i = 1, pick_IO%ntot_pick_spectr
!$omp do
        do nd = 1, pick_IO%ntot_comp
          sdev_spec(nd,i) = rms_spec(nd,i) - ave_spec(nd,i)**2
!
          ave_spec(nd,i) =   ave_spec(nd,i) * acou
          rms_spec(nd,i) =   sqrt(rms_spec(nd,i) * acou)
          sdev_spec(nd,i) =  sqrt(sdev_spec(nd,i) * acou)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!    output time average
!
      do i = 1, pick_IO%ntot_pick_spectr
        do nd = 1, pick_IO%ntot_comp
          pick_IO%d_pk(nd,i) = ave_spec(nd,i)
        end do
      end do
!
      call write_tave_sph_spec_monitor                                  &
     &   (tave_fname, i_step, time, true_start, pick_IO)
!
!    output RMS deviation
!
      do i = 1, pick_IO%ntot_pick_spectr
        do nd = 1, pick_IO%ntot_comp
          pick_IO%d_pk(nd,i) = rms_spec(nd,i)
        end do
      end do
!
      call write_tave_sph_spec_monitor                                  &
     &   (trms_fname, i_step, time, true_start, pick_IO)
!
!    output standard deviation
!
      do i = 1, pick_IO%ntot_pick_spectr
        do nd = 1, pick_IO%ntot_comp
          pick_IO%d_pk(nd,i) = sdev_spec(nd,i)
        end do
      end do
!
      call write_tave_sph_spec_monitor                                  &
     &   (sdev_fname, i_step, time, true_start, pick_IO)
!
      call dealloc_pick_sph_monitor_IO(pick_IO)
      deallocate(prev_spec, ave_spec, sdev_spec)
!
      end subroutine s_time_ave_picked_sph_spectr
!
! -------------------------------------------------------------------
!
      end module time_ave_picked_sph_spectr