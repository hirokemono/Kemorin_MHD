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
      use read_psf_select_4_zlib
!
      use m_line_from_psf
      use m_psf_edge_connect
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
      if(direction .eq. 'x' .or. direction .eq. 'X') nd = 1
      if(direction .eq. 'y' .or. direction .eq. 'Y') nd = 2
      if(direction .eq. 'z' .or. direction .eq. 'Z') nd = 3
!
      if(direction .eq. 'r' .or. direction .eq. 'R') nd = 11
      if(direction .eq. 's' .or. direction .eq. 's') nd = 21
!
      write(*,*) 'input output header'
      read(*,*) line_udt_head
!
      do istep = istep_start, istep_end, istep_int
        call sel_read_alloc_psf_file(iflag_psf_fmt, istep)
        call find_psf_edges
        call pick_psf_by_sections(nd, xref)
!
        call deallocate_psf_results
!
        call write_psf_line_data(iflag_ucd, line_udt_head, istep)
      end do
!
      stop ' //// program normally terminated //// '
!
      end program section_from_psf
