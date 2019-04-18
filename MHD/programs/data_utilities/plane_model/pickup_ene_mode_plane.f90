!
      program pickup_ene_mode_plane
!
      use m_precision
!
      use set_spectr_file_name
!
      use t_size_of_cube
      use t_ctl_data_plane_fft
      use set_list_4_FFT
      use set_plane_spectr_file_head
!
      implicit none
!
!
      type(ctl_data_plane_fft), save :: pfft_c1
      type(field_IO_params), save ::  plane_mesh_file
      type(size_of_cube), save :: c_size1
!
      integer(kind=kint) :: nx_2, ny_2
      integer(kind=kint) :: kx, ky, kx_in, ky_in
      integer(kind=kint) :: num_ene_z
      integer(kind=kint) :: ist, ied, iint, num_fft
      integer :: num_pe

      real   (kind=kreal), dimension(:), allocatable  ::  ene_z
      character(len=kchara  ), dimension(:), allocatable :: fft_name
!
      character(len=kchara) :: z_stacked_name
      character(len=kchara) :: output_name
      integer(kind=kint) :: z_stacked_code = 16
      integer(kind=kint) :: output_code = 15
!
      character(len=kchara) :: tmpchara
      integer(kind=kint) :: itmp, nd, k, istep
!
!     set parameters
!
      write(*,*) ' Dou you prepare folloing data???'
      write(*,*) ' control data for this routine:  ctl_fft'
      write(*,*) ' spectr data:  spectr/ene_horiz_spec.step#.dat'
      write(*,*) ' return key to start'
      read(*,*)
!
!     read outline of mesh
!
      write(*,*) 'read_control_data_fft_plane'
      call read_control_data_fft_plane(pfft_c1)
      call s_set_plane_spectr_file_head(pfft_c1, plane_mesh_file)
      call set_parameters_4_FFT(pfft_c1%t_zfft_ctl, pfft_c1%cube_c_fft, &
     &    c_size1, num_pe, ist, ied, iint)
!
!
       nx_2 = c_size1%nx_all/2 + 1
       ny_2 = c_size1%ny_all/2 + 1
       num_ene_z = nx_2*ny_2
!
       write(*,*) 'nx_2, ny_2', nx_2, ny_2
!
      istep = ist
      call s_set_horiz_ene_file_name(istep, z_stacked_name)
      open (z_stacked_code,  file=z_stacked_name,                       &
     &         form='formatted', status ='unknown')
!
      read(z_stacked_code,*) tmpchara
      read(z_stacked_code,*) num_fft
!
       allocate( fft_name(num_fft) )
       allocate ( ene_z(num_fft) )
       ene_z   = 0.0d0
!
      do nd = 1, num_fft
       read(z_stacked_code,*) fft_name(nd)
      end do
!
      close(z_stacked_code)
!
  10  continue
!
      write(*,*) 'select mode (to finish, imput negative values)'
      read(*,*) kx_in, ky_in
!
      if (kx_in.lt.0 .or. ky_in.lt.0) go to 20
!
      call set_mode_file_name(kx_in, ky_in, output_name)
      open (output_code,file=output_name)
!
      write(output_code,*) 'kx: ',kx_in, ', ky: ',ky_in
      write(output_code,*) 'step'
      do nd = 1, num_fft
       write(output_code,*) trim(fft_name(nd))

      end do
!
      do istep = ist, ied, iint
!
       call s_set_horiz_ene_file_name(istep, z_stacked_name)
       open (z_stacked_code,  file=z_stacked_name,                      &
     &         form='formatted', status ='unknown')
!
       read(z_stacked_code,*) tmpchara
       read(z_stacked_code,*) itmp
       do nd = 1, num_fft
        read(z_stacked_code,*) tmpchara
       end do
!
       do k = 1, num_ene_z
        read(z_stacked_code,*) kx, ky, (ene_z(nd),nd= 1, num_fft)
        if ( kx.eq.kx_in .and. ky.eq.ky_in) then
         write(output_code,'(i15,1p254e23.12)')                         &
     &    istep, (ene_z(nd),nd= 1, num_fft)
        end if
       end do
!
       close(z_stacked_code)
!
      end do
!
      close(output_code)
!
      go to 10
 20   continue
!
!
      stop
      end program pickup_ene_mode_plane
