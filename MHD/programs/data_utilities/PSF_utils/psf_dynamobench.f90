!
!     program psf_dynamobench
!
      program psf_dynamobench
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
!
      use m_line_from_psf
      use m_psf_edge_connect
      use m_dynamobench_data_by_psf
!
      use t_ucd_data
!
      implicit    none
!
      character(len=kchara) :: rms_file_header, line_udt_head
      character(len=kchara) :: file_name, tmpchara1, tmpchara2
!
      integer(kind = kint) :: istep_start, istep_end
      integer(kind = kint) :: istep_int
!
      integer(kind = kint) :: istep, istep_ene, istep_psf
      integer(kind = kint), parameter :: nd = 11, id_rms = 15
      real(kind = kreal) :: xref
      real(kind = kreal) :: time
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
      write(*,*) 'input mean  square header'
      read(*,*) rms_file_header
!
      write(*,*) 'input istep_start, istep_end, istep_int'
      read(*,*) istep_start, istep_end, istep_int
!
      write(*,*) 'input interval for PSF'
      read(*,*) istep_psf
!
      xref = 0.5d0 + 7.0d0 / 13.0d0
      iflag_psf_fmt = iflag_udt_gz
      line_udt_head = 'eq_mid_depth'
!
!
      call add_dat_extension(rms_file_header, file_name)
      open(id_rms, file = file_name, status='old')
!
      read(id_rms,*) tmpchara1, tmpchara2
      if(tmpchara1.ne.'t_step' .or. tmpchara2.ne.'time') then
        close(id_rms)
        stop 'someting is wong in mean square data'
      end if
!
      do istep = istep_start, istep_end, istep_int
!
        do
          read(id_rms,*,end=99) istep_ene, time
          istep_ene = istep_ene / istep_psf
          if(istep_ene .ge. istep) exit
        end do
        write(*,*) 'step is', istep, time
!
        call load_psf_data(istep, psf_u)
        call find_psf_edges(psf_u%psf_ele)
        call pick_psf_by_sections(nd, xref, psf_u%psf_nod,              &
     &      psf_u%psf_ele, psf_u%psf_phys, line)
!
        call deallocate_psf_edge
        call dealloc_psf_results(psf_u)
!
        call cal_dynamobench_data_by_psf(istep, time, line)
!        call write_psf_line_data(iflag_ucd, line_udt_head, istep, line)
        call deallocate_ucd_mesh(line)
      end do
!
  99  continue
      close(id_rms)
      stop ' //// program normally finished //// '
!
      end program psf_dynamobench
