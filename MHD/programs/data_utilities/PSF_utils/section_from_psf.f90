!
!     program section_from_psf
!
!      program for pick data clong line
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
!
      program section_from_psf
!
      use m_precision
      use m_constants
!
      use m_psf_results
      use m_field_file_format
      use set_parallel_file_name
      use skip_comment_f
!
      use m_line_from_psf
      use m_psf_edge_connect
!
      use t_time_data
      use t_ucd_data
!
      implicit    none
!
      character(len=kchara) :: line_udt_head
!
      integer(kind = kint) :: istep_start, istep_end
      integer(kind = kint) :: istep_int
!
      integer(kind = kint) :: istep
      character(len=kchara) :: direction
      integer(kind = kint) :: nd
      real(kind = kreal) :: xref
!
      type(time_data), save :: line_time
      type(ucd_data), save :: line
!
!  ===========
! . for local 
!  ===========
!
      write(*,*) 'Choose psf format'
      write(*,*) iflag_ucd, ': UCD'
      write(*,*) iflag_udt, ': UDT'
      write(*,*) iflag_vtk, ': VTK'
      write(*,*) iflag_ucd_gz, ': gzipped_UCD'
      write(*,*) iflag_udt_gz, ': gzipped_UDT'
      write(*,*) iflag_vtk_gz, ': gzipped_VTK'
!
      read(*,*)  iflag_psf_fmt
      write(*,*) 'iflag_psf_fmt', iflag_psf_fmt
!
      write(*,*) 'input psf file name'
      read(*,*) psf_file_header
!
      psf_u%iflag_psf_fmt =   iflag_psf_fmt
      psf_u%psf_file_header = psf_file_header
!
      write(*,*) 'inputistep_start, istep_end, istep_int'
      read(*,*) istep_start, istep_end, istep_int
!
      write(*,*) 'input direction to cut'
      read(*,*) direction
      write(*,*) 'input position to cut'
      read(*,*) xref
!
      if(cmp_no_case(direction, 'X')) nd = 1
      if(cmp_no_case(direction, 'Y')) nd = 2
      if(cmp_no_case(direction, 'Z')) nd = 3
      if(cmp_no_case(direction, 'r')) nd = 11
      if(cmp_no_case(direction, 's')) nd = 21
!
      write(*,*) 'input output header'
      read(*,*) line_udt_head
!
      do istep = istep_start, istep_end, istep_int
        call load_psf_data(istep, psf_u)
        call find_psf_edges(psf_u%psf_ele)
        call pick_psf_by_sections                                       &
     &     (nd, xref, psf_u%psf_nod, psf_u%psf_ele,                     &
     &      psf_u%psf_phys, line)
!
        call deallocate_psf_edge
        call dealloc_psf_results(psf_u)
!
        call write_psf_line_data                                        &
     &     (iflag_ucd, line_udt_head, istep, line_time, line)
      end do
!
      stop ' //// program normally finished //// '
!
      end program section_from_psf
