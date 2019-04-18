!
!     program to pick up horizontal average
!
!
!--------------------------------------------------------------------
!
      program pickup_mode_plane
!
      use m_precision
!
      use t_size_of_cube
      use t_mesh_data_4_merge
      use t_ctl_data_plane_fft
!
      use set_numnod_4_plane
      use set_spectr_file_name
!
      use set_list_4_FFT
      use set_plane_spectr_file_head
      use set_parallel_file_name
!
      implicit none
!
      type(ctl_data_plane_fft), save :: pfft_c1
      type(field_IO_params), save ::  plane_mesh_file
      type(merged_mesh), save :: mgd_mesh_pm
      type(size_of_cube), save :: c_size1
!
      integer(kind=kint) :: nx_2, ny_2
      integer(kind=kint) :: num_ene, num_ene_z
!
      real   (kind=kreal), allocatable  ::  zz(:)

      real   (kind=kreal), allocatable  ::  phys_cxcy(:,:)
      real   (kind=kreal), allocatable  ::  phys_sxcy(:,:)
      real   (kind=kreal), allocatable  ::  phys_cxsy(:,:)
      real   (kind=kreal), allocatable  ::  phys_sxsy(:,:)

      character(len=kchara), allocatable :: fft_name(:)
      character(len=kchara), allocatable :: fft_comp(:)
!
      integer(kind=kint) :: ist, ied, iint, num_fft
      integer(kind=kint) :: i1, i2, i3, iii
!
      character(len=kchara) :: spectr_data_name
      character(len=kchara) :: output_name
!
      integer(kind=kint), parameter :: spectr_grid_code = 17
      integer(kind=kint), parameter :: spectr_data_code = 19
      integer(kind=kint), parameter :: picked_data_code = 20

      character(len=kchara) :: tmpchara
!
      integer(kind=kint) :: kx_out, ky_out
!
      integer(kind=kint) :: ix, iy, iz, j, istep
      integer :: num_pe
!
!     set parameters
!
      write(*,*) ' Dou you prepare folloing data???'
      write(*,*) ' control data for this routine:  ctl_fft'
      write(*,*) ' spectr data:  spectr/spec_mode.dat'
      write(*,*) ' spectr data:  spectr/spectr.step#.dat'
      write(*,*) ' return key to start'
      read(*,*)
!
!     read outline of mesh
!
      write(*,*) 'read_control_data_fft_plane'
      call read_control_data_fft_plane(pfft_c1)
      write(*,*) 's_set_plane_spectr_file_head'
      call s_set_plane_spectr_file_head(pfft_c1, plane_mesh_file)
      call set_parameters_4_FFT(pfft_c1%t_zfft_ctl, pfft_c1%cube_c_fft, &
     &    c_size1, mgd_mesh_pm%num_pe, ist, ied, iint)
!
!
      call s_set_numnod_4_plane(c_size1, mgd_mesh_pm%merge_tbl)
!
       nx_2 = c_size1%nx_all/2 + 1
       ny_2 = c_size1%ny_all/2 + 1
!
       write(*,*) 'Truncation levels', (nx_2-1), (ny_2-1)
!
       num_ene_z = nx_2*ny_2
       num_ene   = nx_2*ny_2 * c_size1%nz_all
!
!
      istep = ist
       call s_set_spectr_file_name(istep, spectr_data_name)
       open (spectr_data_code,  file=spectr_data_name,                  &
     &         form='formatted', status ='unknown')
!
       read(spectr_data_code,*) tmpchara, num_fft
!
      close(spectr_data_code)
!
!    allocate arrays
!
!       write(*,*) num_ene, num_fft
!
       allocate( zz(c_size1%nz_all) )
!
       allocate( fft_name(num_fft) )
       allocate( fft_comp(num_fft) )
!
       allocate ( phys_cxcy(num_ene,num_fft) )
       allocate ( phys_sxcy(num_ene,num_fft) )
       allocate ( phys_cxsy(num_ene,num_fft) )
       allocate ( phys_sxsy(num_ene,num_fft) )

       phys_cxcy = 0.0d0
       phys_sxcy = 0.0d0
       phys_cxsy = 0.0d0
       phys_sxsy = 0.0d0
!
      open (spectr_grid_code, file=spec_mode_file_name,                 &
     &                       form='formatted', status ='unknown')
!
      read(spectr_grid_code,*)
      do iy = 1, c_size1%nx_all/2
        do ix = 1, c_size1%ny_all/2
          do iz = 1, c_size1%nz_all
            iii = iz * c_size1%nx_all * c_size1%ny_all
            read(spectr_grid_code,*) i1, i2, i3, zz(iz)
          end do
         end do
      end do
!
      close(spectr_grid_code)
!
      call s_set_spectr_file_name(ist, spectr_data_name)
      write(*,*) 'file name:', spectr_data_name
      open (spectr_data_code,  file=spectr_data_name,                   &
     &         form='formatted', status ='unknown')
!
      read(spectr_data_code,*) tmpchara, num_fft
!
!
      do j = 1, num_fft
!
        read(spectr_data_code,*) fft_name(j)
        read(spectr_data_code,*) fft_comp(j)
!
        do iy = 1, ny_2
          do ix = 1, nx_2
            do iz = 1, c_size1%nz_all
              iii   = (iy-1)*(c_size1%nz_all*nx_2)                      &
     &               + (ix-1)*c_size1%nz_all + iz
!          write(*,*) 'tako', j, iy, ix, iz, iii
              read(spectr_data_code,'(1p4e20.11)')                      &
     &                  phys_cxcy(iii,j), phys_sxcy(iii,j),             &
     &                  phys_cxsy(iii,j), phys_sxsy(iii,j)
            end do
          end do
        end do
      end do
!
      close(spectr_data_code)
!
!
      do
!
        write(*,*) 'input wave numbers to pickup (kx, ky)'
        write(*,*) 'To finish enter negative values'
        read(*,*)   kx_out, ky_out
        if ( kx_out.lt.0 .or. ky_out.lt.0) exit
!
        call set_picked_mode_file_name(kx_out, ky_out, output_name)
        open (picked_data_code,  file=output_name,                      &
     &         form='formatted', status ='unknown')
!
        write(picked_data_code,*)   '# number of component'
        write(picked_data_code,*)   num_fft
!
        write(picked_data_code,*)   't_step, '
        write(picked_data_code,*)   'iz, zz, '
!
        do j = 1, num_fft
          write(picked_data_code,*)                                     &
     &        trim(fft_name(j)), '_', trim(fft_comp(j)),'_cxcy, '
        end do
!
        if (kx_out .gt. 0) then
          do j = 1, num_fft
            write(picked_data_code,*)                                   &
     &        trim(fft_name(j)), '_', trim(fft_comp(j)),'_sxcy, '
          end do
        end if
!
        if (ky_out .gt. 0) then
          do j = 1, num_fft
            write(picked_data_code,*)                                   &
     &        trim(fft_name(j)), '_', trim(fft_comp(j)),'_cxsy, '
           end do
        end if
!
        if (kx_out .gt. 0  .and.  ky_out .gt. 0) then
          do j = 1, num_fft
            write(picked_data_code,*)                                   &
     &        trim(fft_name(j)), '_', trim(fft_comp(j)),'_sxsy, '
           end do
        end if
!
!
        do istep = ist, ied, iint
!
!
          call s_set_spectr_file_name(istep, spectr_data_name)
          write(*,*) 'file name:', spectr_data_name
          open (spectr_data_code,  file=spectr_data_name,               &
     &         form='formatted', status ='unknown')
!
          read(spectr_data_code,*) tmpchara, num_fft
!
!
          do j = 1, num_fft
!
            read(spectr_data_code,*) fft_name(j)
            read(spectr_data_code,*) fft_comp(j)
!
            do iy = 1, ny_2
              do ix = 1, nx_2
                do iz = 1, c_size1%nz_all
                  iii   = (iy-1)*(c_size1%nz_all*nx_2)                  &
     &                   + (ix-1)*c_size1%nz_all + iz
!          write(*,*) 'tako', j, iy, ix, iz, iii
                  read(spectr_data_code,'(1p4e20.11)')                  &
     &                  phys_cxcy(iii,j), phys_sxcy(iii,j),             &
     &                  phys_cxsy(iii,j), phys_sxsy(iii,j)
                end do
              end do
            end do
          end do
!
          close(spectr_data_code)
!
!
          if (kx_out .eq. 0  .and.  ky_out .eq. 0) then
            do iz = 1, c_size1%nz_all
              iii   = ky_out*(c_size1%nz_all*nx_2)                      &
     &               + kx_out*c_size1%nz_all + iz
              write(picked_data_code,'(2i16,1p256e20.11)')              &
     &           istep, iz, zz(iz),                                     &
     &           phys_cxcy(iii,1:num_fft)
            end do
!
          else if (ky_out .eq. 0) then
            do iz = 1, c_size1%nz_all
              iii   = ky_out*(c_size1%nz_all*nx_2)                      &
     &               + kx_out*c_size1%nz_all + iz
              write(picked_data_code,'(2i16,1p256e20.11)')              &
     &           istep, iz, zz(iz),                                     &
     &           phys_cxcy(iii,1:num_fft), phys_sxcy(iii,1:num_fft)
            end do
!
          else if (kx_out .eq. 0) then
            do iz = 1, c_size1%nz_all
              iii   = ky_out*(c_size1%nz_all*nx_2)                      &
     &               + kx_out*c_size1%nz_all + iz
              write(picked_data_code,'(2i16,1p256e20.11)')              &
     &           istep, iz, zz(iz),                                     &
     &           phys_cxcy(iii,1:num_fft),                              &
     &           phys_cxsy(iii,1:num_fft)
            end do
          else
            do iz = 1, c_size1%nz_all
              iii   = ky_out*(c_size1%nz_all*nx_2)                      &
     &               + kx_out*c_size1%nz_all + iz
              write(picked_data_code,'(2i16,1p256e20.11)')              &
     &           istep, iz, zz(iz),                                     &
     &           phys_cxcy(iii,1:num_fft), phys_sxcy(iii,1:num_fft),    &
     &           phys_cxsy(iii,1:num_fft), phys_sxsy(iii,1:num_fft)
            end do
          end if
!
        end do
!
        close(picked_data_code)
      end do
!
      stop
      end
