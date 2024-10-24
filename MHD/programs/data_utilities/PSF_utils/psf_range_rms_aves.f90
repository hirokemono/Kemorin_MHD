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
      use m_section_file_extensions
!
      use t_file_IO_parameter
      use t_time_data
      use t_ucd_data
!
      use set_parallel_file_name
      use ucd_IO_select
      use cal_psf_rms_aves
      use take_avarages_4_psf
      use take_normals_4_psf
!
      implicit    none
!
      type(time_data), save :: psf_time
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
      write(*,*) 'input file prefix'
      read(*,*) psf_file_param%file_prefix
      psf_file_param%iflag_format = section_format_id_from_input()
!
      write(*,*) 'input istep_start, istep_end, istep_int'
      read(*,*) istep_start, istep_end, istep_int
!
      write(*,*) 'input radius range'
      read(*,*) rmin, rmax
!
      call load_psf_data_to_link_IO                                     &
     &   (istep_start, psf_file_param, t_IO_u, psf_u, psf_ucd)
      call alloc_psf_averages(psf_u%psf_phys, psf_average)
!
      do i_fld = 1, psf_u%psf_phys%num_phys
        write(*,*) i_fld, psf_u%psf_phys%num_component(i_fld),          &
     &             psf_u%psf_phys%istack_component(i_fld),              &
     &             trim(psf_u%psf_phys%phys_name(i_fld))
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
      icomp_ref_field                                                   &
     &    = psf_u%psf_phys%istack_component(ifield_ref_field-1)         &
     &     + icomp_ref_field
!
!   Evaluate size of patches
!
      call allocate_norms_4_psf                                         &
     &   (psf_u%psf_nod, psf_u%psf_ele, psf_normal)
      call cal_center_ele_4_psf                                         &
     &   (psf_u%psf_nod,  psf_u%psf_ele, psf_normal)
      call cal_norm_area_4_psf                                          &
     &   (psf_u%psf_nod, psf_u%psf_ele, psf_normal)
!
      call set_averaging_range(rmin, rmax, psf_normal)
!
      call open_psf_ave_rms_data                                        &
     &   (psf_file_param%file_prefix, psf_u%psf_phys)
!
      icou = 0
      write(*,'(a,i15)', advance='NO')                                  &
     &          'read for averaging. Step:  ', istep_start
      do istep = istep_start, istep_end, istep_int
        icou = icou + 1
        write(*,'(10a1)', advance='NO') (char(8),i=1,10)
        write(*,'(i15)', advance='NO') istep
!
        call sel_read_udt_file                                          &
     &     (-1, istep, psf_file_param, psf_time, psf_ucd)
        call cal_range_rms_ave_4_psf                                    &
     &     (psf_u%psf_ele, psf_u%psf_phys, psf_normal,                  &
     &      icomp_ref_field, iflag_ref, ref_value, area_res,            &
     &      psf_average)
        call cal_minmax_psf                                             &
     &     (psf_u%psf_nod%numnod, psf_u%psf_phys%ntot_phys,             &
     &      psf_u%psf_phys%d_fld, psf_average)
!
!
        call write_psf_ave_rms_data(istep, area_res, psf_average)
      end do
      write(*,*)
      call close_psf_ave_rms_data
!
      stop ' //// program normally finished //// '
!
      end program psf_range_rms_aves
