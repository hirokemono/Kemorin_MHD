!
!     program psf_range_rms_aves
!
      program psf_range_rms_aves
!
!      program for pick up surface connectivities form subdomain mesh
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
!
      use m_precision
      use m_constants
!
      use m_psf_results
      use m_field_file_format
!
      use t_ucd_data
!
      use set_parallel_file_name
      use ucd_IO_select
      use cal_psf_rms_aves
      use take_avarages_4_psf
      use take_normals_4_psf
      use load_psf_data
!
      implicit    none
!
!
      type(ucd_data), save:: psf_ucd
!
      integer(kind = kint) :: istep_start, istep_end
      integer(kind = kint) :: istep_int
      real(kind = kreal) :: rmin, rmax
!
      integer(kind = kint) :: icomp_ref_field
      integer(kind = kint) :: ifield_ref_field
      integer(kind = kint) :: iflag_ref
      real(kind = kreal) :: ref_value, area_res
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: i, i_fld
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
      write(*,*) 'input istep_start, istep_end, istep_int'
      read(*,*) istep_start, istep_end, istep_int
!
      write(*,*) 'input radius range'
      read(*,*) rmin, rmax
!
!
      call s_load_psf_data(istep_start)
      call set_psf_mesh_to_ucd_data(psf_ucd)
!
      do i_fld = 1, nfield_psf
        write(*,*) i_fld, ncomp_psf(i_fld), istack_comp_psf(i_fld),     &
     &             trim(psf_data_name(i_fld))
      end do
!
      write(*,*) 'input field ID for reference'
      read(*,*)  ifield_ref_field
      write(*,*) 'input component number for reference'
      read(*,*)  icomp_ref_field
      write(*,*) 'input reference value'
      read(*,*)  ref_value
      write(*,*) 'input reference mode'
      write(*,*) ' 1:  More than reference'
      write(*,*) ' 2:  Less than reference'
      read(*,*)  iflag_ref
!
      icomp_ref_field = istack_comp_psf(ifield_ref_field-1)             &
     &                 + icomp_ref_field
!
!   Evaluate size of patches
!
      call allocate_norms_4_psf
      call cal_center_ele_4_psf
      call cal_norm_area_4_psf
!
      call set_averaging_range(rmin, rmax)
!
      call open_psf_ave_rms_data(psf_file_header)
!
      psf_ucd%ifmt_file = iflag_psf_fmt
      psf_ucd%file_prefix = psf_file_header
!
      icou = 0
      write(*,'(a,i10)', advance='NO')                                  &
     &          'read for averaging. Step:  ', istep_start
      do istep = istep_start, istep_end, istep_int
        icou = icou + 1
        write(*,'(10a1)', advance='NO') (char(8),i=1,10)
        write(*,'(i10)', advance='NO') istep
!
        call sel_read_udt_file(iminus, istep, psf_ucd)
        call cal_range_rms_ave_4_psf                                    &
     &     (icomp_ref_field, iflag_ref, ref_value, area_res)
        call cal_minmax_psf
!
!
        call write_psf_ave_rms_data(istep, area_res)
      end do
      write(*,*)
      call close_psf_ave_rms_data
!
      stop ' //// program normally finished //// '
!
      end program psf_range_rms_aves
