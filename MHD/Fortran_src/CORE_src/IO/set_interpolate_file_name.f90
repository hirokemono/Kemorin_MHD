!>@file   set_interpolate_file_name.f90
!!@brief  module set_interpolate_file_name
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      logical function check_exist_interpolate_file                   &
!!     &               (id_rank, table_file_IO)
!!      logical function check_writable_itp_file(id_rank, table_file_IO)
!!
!!      character(len=kchara) function set_mpi_interpolate_file_name    &
!!     &                             (id_rank, table_file_IO)
!!      character(len=kchara) function s_set_interpolate_file_name      &
!!     &                             (id_rank, file_prefix)
!!
!!      character(len=kchara) function set_mpi_interpolate_work_name    &
!!     &                             (id_rank, table_file_IO)
!!      character(len=kchara) function set_interpolate_work_name        &
!!     &                             (id_rank, table_file_IO)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!@endverbatim
!
      module set_interpolate_file_name
!
      use m_precision
!
      use m_file_format_switch
      use t_file_IO_parameter
!
      implicit none
!
      character(len=kchara), parameter, private :: work_header = 'work'
!
      character(len=3), parameter, private :: itp_ext = "itp"
      character(len=3), parameter, private :: itb_ext = "itb"
!
      private :: add_itp_extension, add_itb_extension
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      logical function check_exist_interpolate_file                     &
     &               (id_rank, table_file_IO)
!
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      character(len=kchara) :: file_name
!
!
      check_exist_interpolate_file = .TRUE.
      file_name = set_mpi_interpolate_file_name(id_rank, table_file_IO)
      check_exist_interpolate_file = check_file_exist(file_name)
!
      end function check_exist_interpolate_file
!
!  ---------------------------------------------------------------------
!
      logical function check_writable_itp_file(id_rank, table_file_IO)
!
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      character(len=kchara) :: file_name
!
!
      file_name = set_mpi_interpolate_file_name(id_rank, table_file_IO)
      check_writable_itp_file = check_file_writable(id_rank, file_name)
!
      end function check_writable_itp_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      character(len=kchara) function set_mpi_interpolate_file_name      &
     &                             (id_rank, table_file_IO)
!
      use set_parallel_file_name
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint) :: iflag
      character(len=kchara) :: fname_tmp, file_name
!
      if(table_file_IO%iflag_format/100 .gt. 0) then
        iflag = mod(table_file_IO%iflag_format,100)
        if(   (iflag .eq. id_binary_file_fmt)                           &
     &   .or. (iflag .eq. id_gzip_bin_file_fmt)) then
          fname_tmp =  add_itb_extension(table_file_IO%file_prefix)
        else
          fname_tmp =  add_itp_extension(table_file_IO%file_prefix)
        end if
!
        if(   (iflag .eq. id_gzip_txt_file_fmt)                         &
     &   .or. (iflag .eq. id_gzip_bin_file_fmt)) then
          file_name =  add_gzip_extension(fname_tmp)
        else
          file_name = fname_tmp
        end if
      else
        file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
      end if
      set_mpi_interpolate_file_name = file_name
!
      end function set_mpi_interpolate_file_name
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function s_set_interpolate_file_name        &
     &                             (id_rank, table_file_IO)
!
      use set_parallel_file_name
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      character(len=kchara) :: fname_tmp, file_name
!
      fname_tmp = add_process_id(id_rank, table_file_IO%file_prefix)
      if(     (table_file_IO%iflag_format .eq. id_binary_file_fmt)      &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        file_name =  add_itb_extension(fname_tmp)
      else
        file_name =  add_itp_extension(fname_tmp)
      end if
!
      if(     (table_file_IO%iflag_format .eq. id_gzip_txt_file_fmt)    &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        s_set_interpolate_file_name =  add_gzip_extension(file_name)
      else
        s_set_interpolate_file_name =  file_name
      end if
!
      end function s_set_interpolate_file_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function set_mpi_interpolate_work_name      &
     &                             (id_rank, table_file_IO)
!
      use set_parallel_file_name
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint) :: iflag
      character(len=kchara) :: fname_tmp, file_name
!
      if(table_file_IO%iflag_format/100 .gt. 0) then
        iflag = mod(table_file_IO%iflag_format,100)
        if(   (iflag .eq. id_binary_file_fmt)                           &
     &   .or. (iflag .eq. id_gzip_bin_file_fmt)) then
          fname_tmp =  add_itb_extension(work_header)
        else
          fname_tmp =  add_itp_extension(work_header)
        end if
!
        if(   (iflag .eq. id_gzip_txt_file_fmt)                         &
     &   .or. (iflag .eq. id_gzip_bin_file_fmt)) then
          file_name =  add_gzip_extension(fname_tmp)
        else
          file_name = fname_tmp
        end if
      else
        file_name = set_interpolate_work_name(id_rank, table_file_IO)
      end if
      set_mpi_interpolate_work_name = file_name
!
      end function set_mpi_interpolate_work_name
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function set_interpolate_work_name          &
     &                             (id_rank, table_file_IO)
!
      use set_parallel_file_name
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      character(len=kchara) :: fname_tmp, file_name
!
      fname_tmp = add_process_id(id_rank, work_header)
      if(     (table_file_IO%iflag_format .eq. id_binary_file_fmt)      &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        file_name =  add_itb_extension(fname_tmp)
      else
        file_name =  add_itp_extension(fname_tmp)
      end if
!
      if(     (table_file_IO%iflag_format .eq. id_gzip_txt_file_fmt)    &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        set_interpolate_work_name =  add_gzip_extension(file_name)
      else
        set_interpolate_work_name =  file_name
      end if
!
      end function set_interpolate_work_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_itp_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_itp_extension = add_3chara_extension(file_head, itp_ext)
!
      end function add_itp_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_itb_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_itb_extension = add_3chara_extension(file_head, itb_ext)
!
      end function add_itb_extension
!
!-----------------------------------------------------------------------
!
      end module set_interpolate_file_name
