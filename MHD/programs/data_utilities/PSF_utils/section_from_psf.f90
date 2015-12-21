!
!     program section_from_psf
!
      program section_from_psf
!
!      program for pick data clong line
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
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
      type(ucd_data), save :: line
!
!  ===========
! . for local 
!  ===========
!
      write(*,*) 'input psf file name'
      read(*,*) psf_file_header
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
        call load_psf_data(istep)
        call find_psf_edges
        call pick_psf_by_sections(nd, xref, line)
!
        call deallocate_psf_edge
        call deallocate_psf_results
!
        call write_psf_line_data(iflag_ucd, line_udt_head, istep, line)
      end do
!
      stop ' //// program normally finished //// '
!
      end program section_from_psf
