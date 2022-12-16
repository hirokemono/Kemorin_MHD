!>@file   sph_layer_monitor_snap_IO.f90
!!        module sph_layer_monitor_snap_IO
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine write_sph_layer_spectr_snapshot(flag_gzip, file_name,&
!!     &          comment, sph_IN, spectr_IO)
!!        logical, intent(in) :: flag_gzip
!!        character(len = kchara), intent(in) :: file_name
!!        character(len = *), intent(in) :: comment
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: spectr_IO(sph_IN%ntot_sph_spec,           &
!!     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
!!      subroutine read_sph_layer_spectr_snapshot(fname_org, sph_lbl_IN,&
!!     &                                          sph_IN, spectr_IO)
!!        character(len = kchara), intent(in) :: fname_org
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: spectr_IO(sph_IN%ntot_sph_spec,           &
!!     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
!!
!!      subroutine write_sph_layer_mean_snapshot(flag_gzip, file_name,  &
!!     &          comment, monitor_labels, sph_IN, spectr_IO, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        character(len = kchara), intent(in) :: file_name
!!        character(len = *), intent(in) :: comment
!!        type(sph_spectr_head_labels), intent(in) :: monitor_labels
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!        real(kind = kreal), intent(in)                                &
!!     &               :: spectr_IO(sph_IN%ntot_sph_spec,sph_IN%nri_sph)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine read_sph_layer_mean_snapshot(fname_org, sph_lbl_IN,  &
!!     &                                        sph_IN, spectr_IO)
!!        character(len = kchara), intent(in) :: fname_org
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        real(kind = kreal), intent(inout)                             &
!!     &               :: spectr_IO(sph_IN%ntot_sph_spec,sph_IN%nri_sph)
!!@endverbatim
!
      module sph_layer_monitor_snap_IO
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_stream = 46
      integer(kind = kint), parameter, private :: id_read_rms = 45
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_layer_spectr_snapshot(flag_gzip, file_name,  &
     &          comment, sph_IN, spectr_IO)
!
      use set_parallel_file_name
      use gz_open_sph_monitor_file
      use gz_layer_spectr_monitor_IO
!
      logical, intent(in) :: flag_gzip
      character(len = kchara), intent(in) :: file_name
      character(len = *), intent(in) :: comment
      type(read_sph_spectr_data), intent(in) :: sph_IN
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_IO(sph_IN%ntot_sph_spec,             &
     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
!
      type(buffer_4_gzip) :: zbuf_wt
!
!
      write(*,*) 'Write file: ', trim(file_name)
      open(id_stream, file=file_name, status='replace',                 &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      write(id_stream) comment
      call write_sph_pwr_layer_head(flag_gzip, id_stream,               &
     &                              sph_pwr_labels, sph_IN, zbuf_wt)
      call sel_gz_write_layer_spectr_mtr                                &
     &   (flag_gzip, id_stream, sph_IN%i_step, sph_IN%time,             &
     &    sph_IN%nri_sph, sph_IN%kr_sph, sph_IN%r_sph, sph_IN%ltr_sph,  &
     &    sph_IN%ntot_sph_spec, spectr_IO(1,0,1), zbuf_wt)
      close(id_stream)
!
      end subroutine write_sph_layer_spectr_snapshot
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_layer_spectr_snapshot(fname_org, sph_lbl_IN,  &
     &                                          sph_IN, spectr_IO)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
!
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(sph_IN%ntot_sph_spec,             &
     &                                0:sph_IN%ltr_sph,sph_IN%nri_sph)
!
      integer(kind = kint) :: ierr
      logical, parameter :: current_fmt = .FALSE.
      logical :: flag_gzip1
      character, pointer :: FPz_f1
      type(buffer_4_gzip) :: zbuf_rd
!
!  Read spectr data file
!
      write(*,*) 'Open file: ', trim(fname_org)
      call sel_open_read_gz_stream_file(FPz_f1, id_read_rms,            &
     &                                  fname_org, flag_gzip1, zbuf_rd)
!
      sph_IN%num_time_labels = 5
      call gz_read_sph_pwr_layer_head(FPz_f1, id_read_rms, flag_gzip1,  &
     &                                sph_lbl_IN, sph_IN, zbuf_rd)
!
      call alloc_sph_espec_name(sph_IN)
      call sel_read_sph_spectr_name(FPz_f1, id_read_rms, flag_gzip1,    &
     &   sph_IN%nfield_sph_spec, sph_IN%num_labels,                     &
     &   sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf_rd)
!
      sph_IN%nri_dat = sph_IN%nri_sph
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      call sel_gz_input_sph_layer_spec                                  &
     &   (FPz_f1, id_read_rms, flag_gzip1, current_fmt,                 &
     &    sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,         &
     &    sph_IN%i_step, sph_IN%time, sph_IN%kr_sph,                    &
     &    sph_IN%r_sph, sph_IN%i_mode, spectr_IO(1,0,1), zbuf_rd, ierr)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_rms, flag_gzip1, zbuf_rd)
!
      end subroutine read_sph_layer_spectr_snapshot
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_sph_layer_mean_snapshot(flag_gzip, file_name,    &
     &          comment, monitor_labels, sph_IN, spectr_IO, zbuf)
!
      use set_parallel_file_name
      use gz_open_sph_monitor_file
      use gz_layer_mean_monitor_IO
!
      logical, intent(in) :: flag_gzip
      character(len = kchara), intent(in) :: file_name
      character(len = *), intent(in) :: comment
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(read_sph_spectr_data), intent(in) :: sph_IN
      real(kind = kreal), intent(in)                                    &
     &                :: spectr_IO(sph_IN%ntot_sph_spec,sph_IN%nri_sph)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      write(*,*) 'Write file: ', trim(file_name)
      open(id_stream, file=file_name, status='replace',                 &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      write(id_stream) comment
      call write_sph_pwr_layer_head(flag_gzip, id_stream,               &
     &                              monitor_labels, sph_IN, zbuf)
      call sel_gz_write_layer_mean_mtr                                  &
     &   (flag_gzip, id_stream, sph_IN%i_step, sph_IN%time,             &
     &    sph_IN%nri_sph, sph_IN%kr_sph, sph_IN%r_sph,                  &
     &    sph_IN%ntot_sph_spec, spectr_IO(1,1), zbuf)
      close(id_stream)
!
      end subroutine write_sph_layer_mean_snapshot
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_layer_mean_snapshot(fname_org, sph_lbl_IN,    &
     &                                        sph_IN, spectr_IO)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use gz_layer_mean_monitor_IO
!
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      real(kind = kreal), intent(inout)                                 &
     &                :: spectr_IO(sph_IN%ntot_sph_spec,sph_IN%nri_sph)
!
      integer(kind = kint) :: ierr
      logical, parameter :: current_fmt = .FALSE.
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
      character, pointer :: FPz_f1
!
!  Read spectr data file
!
      write(*,*) 'Open file ', trim(fname_org)
      call sel_open_read_gz_stream_file(FPz_f1, id_read_rms,            &
     &                                    fname_org, flag_gzip1, zbuf1)
!
      sph_IN%num_time_labels = 4
      call gz_read_sph_pwr_layer_head(FPz_f1, id_read_rms, flag_gzip1,  &
     &                                sph_lbl_IN, sph_IN, zbuf1)
!
      call alloc_sph_espec_name(sph_IN)
      call sel_read_sph_spectr_name(FPz_f1, id_read_rms, flag_gzip1,    &
     &   sph_IN%nfield_sph_spec, sph_IN%num_labels,                     &
     &   sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf1)
!
      sph_IN%nri_dat = sph_IN%nri_sph
      call alloc_sph_spectr_data(izero, sph_IN)
!
      call sel_gz_input_sph_layer_mean                                  &
     &   (FPz_f1, id_read_rms, flag_gzip1, current_fmt,                 &
     &    sph_IN%nri_sph, sph_IN%ntot_sph_spec, sph_IN%i_step,          &
     &    sph_IN%time, sph_IN%kr_sph, sph_IN%r_sph,                     &
     &    spectr_IO(1,1), zbuf1, ierr)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_rms, flag_gzip1, zbuf1)
!
      end subroutine read_sph_layer_mean_snapshot
!
!   --------------------------------------------------------------------
!
      end module sph_layer_monitor_snap_IO
