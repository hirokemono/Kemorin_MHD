      program update_old_psf_binary
!
      use m_precision
      use m_constants
      use m_field_file_format
      use m_section_file_extensions
!
      use t_file_IO_parameter
      use t_ucd_data
      use ucd_IO_select
      use sel_read_old_binary_ucd
!
      implicit none
!
      character(len = kchara) :: prefix_org, prefix_new
      real(kind = kreal) :: time_init
!
      integer(kind = kint) :: istep, istart, iend, increment
      type(field_IO_params) :: ucd_param
      type(time_data), save :: t_IO
      type(ucd_data), save :: ucd
      character(len = kchara) :: file_ext
!
!
      ucd_param%iflag_IO = 0
      write(*,*) 'Input original file prefix'
      read(*,*) prefix_org
      write(*,*) 'Input updated file prefix'
      read(*,*) prefix_new
!
      write(*,*) 'Input Initial time'
      read(*,*) time_init
      write(*,*) 'Input time increment'
      read(*,*) t_IO%dt
!
  90  continue
      write(*,*) 'Input file extension including "gz":'
      write(*,*) psf_to_vtk_format_list()
      read(*,*) file_ext
      ucd_param%iflag_format                                            &
     &      = psf_to_vtk_format_id_from_input(file_ext)
      write(*,*) 'iflag_format', ucd_param%iflag_format
      if(ucd_param%iflag_format .eq. iflag_vtk) then
        write(*,*) 'Set correct file extension (except for vtk)'
        go to 90
      end if
!
      write(*,*) 'Input start, end, and increment of file step'
      read(*,*) istart, iend, increment
!
      do istep = istart, iend, increment
        ucd_param%file_prefix = prefix_org
        call sel_read_alloc_old_ucd_file(-1, istep, ucd_param, ucd)
!
        t_IO%time =        time_init + real(istep) * t_IO%dt
        t_IO%i_time_step = istep
!
        ucd_param%file_prefix = prefix_new
        call sel_write_ucd_file(-1, istep, ucd_param, t_IO, ucd)
        call deallocate_ucd_mesh(ucd)
      end do
!
      end program update_old_psf_binary
