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
      use t_spectr_4_ispack
      use t_size_of_cube
      use t_time_data
      use t_ucd_data
      use t_mesh_data_4_merge
      use t_ctl_data_plane_fft
!
      use set_geometry_to_merge
      use set_numnod_4_plane
      use read_udt_data_4_FFT
      use set_list_4_FFT
      use cal_fft_for_horizontal
      use set_plane_spectr_file_head
      use const_merged_groups

      implicit    none
!
!  ===========
! . for local 
!  ===========

      type(ctl_data_plane_fft), save :: pfft_c1
      type(plane_spectr_by_ispack), save :: plane_fft_wk1
      type(field_IO_params), save ::  plane_mesh_file, ucd_file_param
!
      type(size_of_cube), save :: c_size1
      type(time_data), save :: fft_t_IO
      type(ucd_data), save :: fft_ucd
!
      type(merged_mesh), save :: mgd_mesh_pm
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
      call read_control_data_fft_plane(pfft_c1)
!
!     read outline of mesh
!
      call s_set_plane_spectr_file_head(pfft_c1, plane_mesh_file)
      call set_parameters_4_FFT(pfft_c1%t_zfft_ctl, pfft_c1%cube_c_fft, &
     &    c_size1, plane_fft_wk1, mgd_mesh_pm%num_pe, ist, ied, iint)
!
      call s_set_numnod_4_plane(c_size1, mgd_mesh_pm%merge_tbl)
!
!
      call alloc_number_of_mesh(mgd_mesh_pm)
!
!   read field name and number of components
!
      write(*,*) 'init_ucd_data_4_FFT'
      call init_ucd_data_4_FFT(ist, ucd_file_param, fft_t_IO, fft_ucd)
!
      call const_fields_4_FFT                                           &
     &   (pfft_c1%fld_zfft_ctl%field_ctl, plane_fft_wk1)
!
      write(*,*) 'internal_node, ele',                                  &
     &           mgd_mesh_pm%merge_tbl%inter_nod_m,                     &
     &           mgd_mesh_pm%merge_tbl%inter_ele_m
!
!     array allocation
!
      call alloc_geometry_data_4_merge(mgd_mesh_pm)
!
      call alloc_horiz_spectr(plane_fft_wk1)
      call alloc_spectr_4_io(plane_fft_wk1)
!
!  set mesh_information
!
!       write(*,*) 'set_geometry_data_2_merge'
       call set_geometry_data_2_merge(mgd_mesh_pm)
!
!   loop for time integration
!
      do istep = ist, ied, iint
!
       call s_read_udt_data_4_FFT                                       &
     &    (istep, ucd_file_param, mgd_mesh_pm, plane_fft_wk1,           &
     &     fft_t_IO, fft_ucd)
!
!  -------   Fourier Transform
!
       call s_cal_fft_for_horizontal(plane_fft_wk1%kx_max,              &
     &     plane_fft_wk1%ky_max, plane_fft_wk1%iz_max,                  &
     &     plane_fft_wk1%num_spectr, plane_fft_wk1%num_io,              &
     &     plane_fft_wk1%num_fft, plane_fft_wk1%icomp_fft,              &
     &     plane_fft_wk1%phys_d, plane_fft_wk1%wk_pfft,                 &
     &     plane_fft_wk1%phys_io)
!
!
!     ======================
!      data output
!     ======================

      if (istep .eq. ist) then
        call write_size_of_spectr(mgd_mesh_pm%merged, plane_fft_wk1)
      end if
!
      call write_spectr_data(istep, plane_fft_wk1)
!
      write(*,*) 'step', istep, 'finish '
      end do

      stop ' //// program normally finished //// '
!
      end program fft_4_plane 
