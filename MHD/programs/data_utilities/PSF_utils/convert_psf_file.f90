!
!      module convert_psf_file
!
      module convert_psf_file
!
!      Written by H. Matsui on May., 2009
!
      use m_precision
      use m_constants
      use t_ucd_data
!
      implicit none
!
      type(ucd_data), save:: psf_ucd
!
      integer(kind = kint), parameter :: id_psf_result = 7
      integer(kind = kint), parameter :: id_min_psf = 21
      integer(kind = kint), parameter :: id_max_psf = 22
      character(len=kchara), parameter :: fname_min_psf = 'psf_min.dat'
      character(len=kchara), parameter :: fname_max_psf = 'psf_max.dat'
!
      character(len=kchara) :: node_file_name, conn_file_name
!
      private :: id_min_psf, fname_min_psf
      private :: id_max_psf, fname_max_psf
      private :: id_psf_result, node_file_name, conn_file_name
!
!      subroutine s_convert_psf_file
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_convert_psf_file(iflag_convert)
!
      use m_control_params_4_psf
      use m_geometry_constants
      use m_geometry_parameter
      use m_psf_results
      use m_t_step_parameter
      use m_field_file_format
!
      use set_control_visualizer
      use read_psf_select_4_zlib
      use take_avarages_4_psf
      use set_parallel_file_name
      use set_ucd_file_names
      use ucd_type_IO_select
      use dx_grid
      use dx_phys
      use vtk_file_IO
!
      integer(kind = kint), intent(in) :: iflag_convert
!
      integer(kind = kint) :: istep, i_psf
      character(len=kchara) :: file_name
!
!
!
      write(*,*) 'average file name: psf_ave.dat'
      write(*,*) 'RMS file name:     psf_rms.dat'
!
      open(id_min_psf, file=fname_min_psf, form='formatted')
      open(id_max_psf, file=fname_max_psf, form='formatted')
!
!
      do i_psf = 1, num_psf
!
!   read grid data
!
        psf_file_header = psf_header(i_psf)
        call sel_read_alloc_psf_file(iflag_psf_fmt, i_step_init)
        call set_psf_mesh_to_ucd_data(psf_ucd)
!
        call allocate_norms_4_psf
!
!   output grid data
!
        if ( iflag_convert .eq. 0 ) then
          call set_single_grd_file_name(psf_header(i_psf), iflag_vtd,   &
     &        file_name)
!
          call write_vtk_grid(file_name, id_psf_result,                 &
     &        numnod_psf, numele_psf, num_triangle, xx_psf, ie_psf)
        else if ( iflag_convert .eq. 2 ) then
          call add_dx_extension(psf_header(i_psf), node_file_name)
          call add_connect_extension(psf_header(i_psf), conn_file_name)
!
          call write_dx_grid(numnod_psf, numele_psf, num_triangle,      &
     &        numnod_psf, numele_psf, xx_psf, ie_psf,                   &
     &        id_psf_result, node_file_name, conn_file_name)
        end if
!
        do istep = i_step_init, i_step_number, i_step_output_psf
!
!     read PSF field data
          psf_ucd%itype_data_file = iflag_udt
          psf_ucd%header_name = psf_header(i_psf)
          if(istep .ne. i_step_init) then
            call sel_read_udt_type_file(-1, istep, psf_ucd)
          end if
          call cal_minmax_psf
!
!      write converted data
!
          if (iflag_convert .eq. 0) then
            call set_single_ucd_file_name(psf_header(i_psf), iflag_vtd, &
     &          istep, file_name)
!
            call write_vtk_phys(file_name, id_psf_result,               &
     &          numnod_psf, nfield_psf, ncomptot_psf,                   &
     &          ncomp_psf, psf_data_name, d_nod_psf)
          else if (iflag_convert .eq. 2) then
            call add_dx_extension(psf_header(i_psf), node_file_name)
            call add_connect_extension(psf_header(i_psf),               &
     &          conn_file_name)
!
            call write_dx_header(istep, num_triangle, numnod_psf,       &
     &          numele_psf, nfield_psf, ncomp_psf,                      &
     &          psf_data_name, id_psf_result, psf_header(i_psf),        &
     &          node_file_name, conn_file_name)
            call write_dx_phys(istep, numnod_psf, numnod_psf,           &
     &          nfield_psf, ncomptot_psf, ncomp_psf,                    &
     &          d_nod_psf, id_psf_result, psf_header(i_psf) )
          end if
!
!      write min_max data
!
          if( i_psf.eq.1 .and. istep.eq.i_step_init) then
            call write_headers_psf_comp_name(id_min_psf)
            write(id_min_psf,*)
!
            call write_headers_psf_comp_name(id_max_psf)
            write(id_max_psf,*)
          end if
!
          write(id_min_psf,'(2i10,1p255E25.15e3)') i_psf, istep,        &
     &           xmin_psf(1:ncomptot_psf)
          write(id_max_psf,'(2i10,1p255E25.15e3)') i_psf, istep,        &
     &           xmax_psf(1:ncomptot_psf)
!
        end do
!
        call deallocate_norms_4_psf
        call deallocate_psf_results
      end do
!
      close(id_min_psf)
      close(id_max_psf)
!
      end subroutine s_convert_psf_file
!
!-----------------------------------------------------------------------
!
      end module convert_psf_file
