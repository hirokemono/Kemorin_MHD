!
!      module m_correlate_4_plane
!
!      Written by Kemorin
!
!       subroutine open_correlate_files_plane
!       subroutine open_correlate_files_snap(istep)
!       subroutine close_results_4_correlate
!
      module m_correlate_4_plane
!
      use m_precision
!
      use m_file_format_switch
      use m_field_file_format
      use t_file_IO_parameter
      use t_correlate_4_plane
!
      implicit    none
!
      type(correlate_4_plane), save :: pcor1
!
      type(field_IO_params), save ::  cor_mesh_file
      type(field_IO_params), save ::  ref_mesh_file
!
      type(field_IO_params), save :: cor_ucd_param
      type(field_IO_params), save :: ref_ucd_param
!
      character(len=kchara), parameter :: cor_udt_header =  'field/out'
      character(len=kchara), parameter                                  &
     &     :: ref_udt_header =  'field_ref/out'
!
      integer(kind=kint ), parameter :: crt_data_code = 25
      integer(kind=kint ), parameter :: rms_data_code = 27
      character(len=kchara) :: crt_rst_name
      character(len=kchara) :: rms_rst_name
!
      character(len=kchara), parameter :: crt_rst_header = 'correlate'
      character(len=kchara), parameter :: rms_rst_header = 'rms_ratio'
!
      private :: crt_rst_header, rms_rst_header
      private :: crt_rst_name, rms_rst_name
!
      private :: write_header_4_correlate_snap
      private :: write_header_4_correlate_plane
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine open_correlate_files_plane
!
      use set_parallel_file_name
!
      crt_rst_name = add_dat_extension(crt_rst_header)
      rms_rst_name = add_dat_extension(rms_rst_header)
!
      open (crt_data_code, file=crt_rst_name)
      open (rms_data_code, file=rms_rst_name)
!
      call write_header_4_correlate_plane(crt_data_code, pcor1)
      call write_header_4_correlate_plane(rms_data_code, pcor1)
!
      end subroutine open_correlate_files_plane
!
!  ---------------------------------------------------------------------
!
      subroutine open_correlate_files_snap(istep)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep
      character(len=kchara) :: fname_tmp
!
      fname_tmp =    add_int_suffix(istep, crt_rst_header)
      crt_rst_name = add_dat_extension(fname_tmp)
!
      fname_tmp =    add_int_suffix(istep, rms_rst_header)
      rms_rst_name = add_dat_extension(fname_tmp)
!
      open (crt_data_code, file=crt_rst_name)
      open (rms_data_code, file=rms_rst_name)
!
      call write_header_4_correlate_snap(crt_data_code, pcor1)
      call write_header_4_correlate_snap(rms_data_code, pcor1)
!
       end subroutine open_correlate_files_snap
!
!  ---------------------------------------------------------------------
!
      subroutine write_header_4_correlate_plane(file_id, pcor)
!
      integer(kind = kint), intent(in) :: file_id
      type(correlate_4_plane), intent(inout) :: pcor
      integer(kind = kint) :: j
!
       write(file_id,*) 'number of component'
       write(file_id,*) pcor%num_crt
       write(file_id,*) 'step  iz  zz '
       do j = 1, pcor%num_crt
         write(file_id,*) trim(pcor%crt_name(j)), '_',                  &
     &                 trim(pcor%crt_comp(j))
       end do
!
       end subroutine write_header_4_correlate_plane
!
!  ---------------------------------------------------------------------
!
       subroutine write_header_4_correlate_snap(file_id, pcor)
!
      integer(kind = kint), intent(in) :: file_id
      type(correlate_4_plane), intent(in) :: pcor
!
      integer(kind = kint) :: j
!
        write(file_id,*) 'number of component'
        write(file_id,*) pcor%num_crt
        write(file_id,*) 'step  ix iy iz  xx yy zz'
        do j = 1, pcor%num_crt
         write(file_id,*) trim(pcor%crt_name(j)), '_',                  &
     &         trim(pcor%crt_comp(j))
        end do
!
       end subroutine write_header_4_correlate_snap
!
!  ---------------------------------------------------------------------
!
       subroutine close_results_4_correlate
!
       close( crt_data_code )
       close( rms_data_code )
!
       end subroutine close_results_4_correlate
!
!  ---------------------------------------------------------------------
!
      end module m_correlate_4_plane
