!
! ----- program  fft_4_plane
!
!    constract spectr data from simulation results 
!     By H. Matsui
!
      program    fft_4_plane 
!
      use m_precision
!
      use m_size_4_plane
      use m_spectr_4_ispack
      use m_control_plane_fft
      use m_ctl_data_4_plane_model
      use m_geometry_data_4_merge
!
      use t_time_data_IO
      use t_ucd_data
!
      use set_geometry_to_merge
      use set_numnod_4_plane
      use read_udt_data_4_FFT
      use set_list_4_FFT
      use cal_fft_for_horizontal
      use set_plane_spectr_file_head

      implicit    none
!
!  ===========
! . for local 
!  ===========

      type(field_IO_params), save ::  plane_mesh_file
!
      type(time_data), save :: fft_t_IO
      type(ucd_data), save :: fft_ucd
!
      integer(kind=kint ) :: istep
      integer(kind=kint ) :: ist, ied, iint

! ==============================================
! * get number of  nodes,elements for whole PES
! ==============================================

      write(*,*) ' Dou you prepare folloing data???'
      write(*,*) ' control data for this routine:  ctl_fft'
      write(*,*) ' mesh data (default):  mesh/in.PE#'
      write(*,*) ' field data  (default): field/out.step#.PE#.udt'
      write(*,*) ' directory for spectr data  (default):  spectr/'
      write(*,*) ' hit return'
      read(*,*)

!  set parameters for results
!
      write(*,*) 'read_control_data_fft_plane'
      call read_control_data_fft_plane
!
!     read outline of mesh
!
      call s_set_plane_spectr_file_head(plane_mesh_file)
      call set_parameters_4_FFT(num_pe, ist, ied, iint)
!
      call s_set_numnod_4_plane
!
!
      call allocate_number_of_mesh
      call allocate_subdomain_grp_stack
!
!   read field name and number of components
!
      write(*,*) 'init_ucd_data_4_FFT'
      call init_ucd_data_4_FFT(ist, fft_t_IO, fft_ucd)
!
      call set_fields_4_FFT(fld_zfft_ctl%field_ctl)
!
      write(*,*) 'internal_node, ele',                                  &
     &           merge_tbl%inter_nod_m,  merge_tbl%inter_ele_m
!
!     array allocation
!
      call allocate_geometry_data_4_merge
!
      call allocate_horiz_spectr
!
      call allocate_spectr_4_io
!
!  set mesh_information
!
!       write(*,*) 'set_geometry_data_2_merge'
       call set_geometry_data_2_merge
!
!   loop for time integration
!
      do istep = ist, ied, iint
!
       call s_read_udt_data_4_FFT(istep, fft_t_IO, fft_ucd)
!
!  -------   Fourier Transform
!
       call s_cal_fft_for_horizontal
!
!
!     ======================
!      data output
!     ======================

      if (istep .eq. ist) then
        call write_size_of_spectr
      end if
!
       call write_spectr_data(istep)
!
      close(  spectr_data_code )
!
      write(*,*) 'step', istep, 'finish '
      end do

      stop ' //// program normally finished //// '
!
      end program fft_4_plane 
