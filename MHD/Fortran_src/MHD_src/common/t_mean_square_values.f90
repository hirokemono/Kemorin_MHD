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
!!
!!      subroutine output_monitor_file                                  &
!!     &         (my_rank, i_step_MHD, time, iphys, fem_msq, msq_list)
!!        real(kind = kreal), intent(in) :: time
!!        type(phys_data), intent(in) :: nod_fld
!!        type(mean_square_values), intent(in) :: fem_msq
!!      subroutine skip_time_step_data                                  &
!!     &         (my_rank, i_step_MHD, i_step_init, rms_step, fem_msq)
!!        type(IO_step_param), intent(in) :: rms_step
!!        type(mean_square_values), intent(inout) :: fem_msq
!!@endverbatim
!
      module t_mean_square_values
!
      use m_precision
      use t_phys_address
      use t_mean_square_filed_list
!
      implicit  none
!
!
!>        File ID for average data
      integer(kind=kint), parameter :: time_step_data_code = 41
!>        File ID for mean square data
      integer(kind=kint), parameter :: rms_data_code =       43
!
! 
!>      Structure for mean square values
      type mean_square_values
!>        File name for average data
        character(len=kchara)                                           &
     &       :: volume_ave_file_name =     'time_step_data.dat'
!>        File name for mean square data
        character(len=kchara)                                           &
     &       :: volume_rms_file_name =     'time_rms_data.dat'
!
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
      end type mean_square_values
!
!
!>      Structure for mean square addresses not listed in phys_address
      type mean_square_address
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
      end type mean_square_address
!
      private :: time_step_data_code,  rms_data_code
      private :: open_monitor_file
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
! ----------------------------------------------------------------------
!
      subroutine output_monitor_file                                    &
     &         (my_rank, i_step_MHD, time, iphys, fem_msq, msq_list)
!
      use t_phys_data
!
      integer (kind=kint), intent(in) :: my_rank
      integer(kind=kint), intent(in) :: i_step_MHD
      real(kind = kreal), intent(in) :: time
!
      type(phys_address), intent(in) :: iphys
      type(mean_square_values), intent(in) :: fem_msq
      type(mean_square_list), intent(in) :: msq_list
!
!
      if ( my_rank .gt. 0 ) return
!
      call open_monitor_file                                            &
     &   (my_rank, iphys, fem_msq, msq_list)
!
      write(time_step_data_code,'(i16,1p1000e20.11)')                   &
     &     i_step_MHD, time, fem_msq%ave_global(1:fem_msq%num_ave)
      write(rms_data_code,'(i16,1p100e20.11)')                          &
     &     i_step_MHD, time, fem_msq%rms_global(1:fem_msq%num_rms)
!
      close(time_step_data_code)
      close(rms_data_code)
!
      end subroutine output_monitor_file
!
! ----------------------------------------------------------------------
!
      subroutine open_monitor_file                                      &
     &         (my_rank, iphys, fem_msq, msq_list)
!
      use t_phys_data
      use time_step_file_IO
!
      integer (kind=kint), intent(in) :: my_rank
      type(phys_address), intent(in) :: iphys
      type(mean_square_list), intent(in) :: msq_list
      type(mean_square_values), intent(in) :: fem_msq
!
!
      if ( my_rank .ne. 0 ) return
!
!   If data files exist, append data at the end of file
!
      open (time_step_data_code,file = fem_msq%volume_ave_file_name,    &
     &      status='old', position='append', err = 99)
      open (rms_data_code,file = fem_msq%volume_rms_file_name,          &
     &      status='old', position='append', err = 98)
      return
!
!   If data files does not exist, create new data file
!
   98 continue
      close(time_step_data_code)
   99 continue
!
      open (time_step_data_code,file = fem_msq%volume_ave_file_name,    &
     &      status='replace')
      open (rms_data_code,file = fem_msq%volume_rms_file_name,          &
     &      status='replace')
!
      call write_monitor_labels                                         &
     &   (time_step_data_code, rms_data_code, iphys, msq_list)
!
      end subroutine open_monitor_file
!
! ----------------------------------------------------------------------
!
      subroutine skip_time_step_data                                    &
     &         (my_rank, i_step_MHD, i_step_init, rms_step, fem_msq)
!
      use t_IO_step_parameter
!
      integer (kind=kint), intent(in) :: my_rank
      integer(kind=kint), intent(in) :: i_step_MHD, i_step_init
      type(IO_step_param), intent(in) :: rms_step
!
      type(mean_square_values), intent(inout) :: fem_msq
!
      integer (kind = kint) :: i, iflag, i_read_step
      real(kind = kreal) :: rtmp
!
!
      if(my_rank .gt. 0) return
      iflag = i_step_init - mod(i_step_MHD, rms_step%increment)
!
      do
        read(time_step_data_code,*,err=99,end=99)                       &
     &            i_read_step, rtmp, (rtmp,i=1,fem_msq%num_ave)
        if (i_read_step .ge. i_step_init) exit
      end do
 99   continue
!
      do
        read(rms_data_code,*,err=98,end=98)                             &
     &            i_read_step, rtmp, (rtmp,i=1,fem_msq%num_rms)
        if (i_read_step .ge. iflag) exit
      end do
 98   continue
!
      end subroutine skip_time_step_data
!
!  ---------------------------------------------------------------------
!
      end module t_mean_square_values
