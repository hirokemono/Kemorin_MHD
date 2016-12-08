      program lead_energy
!-------------------------------------------------------------------
!
!     program to pick up horizontal average
!
!--------------------------------------------------------------------
!
      use m_precision
!
      use m_phys_labels
      use m_size_4_plane
      use set_numnod_4_plane
      use set_spectr_file_name
      use m_read_mesh_data
!
      use m_control_plane_fft
      use set_list_4_FFT
      use set_plane_spectr_file_head
      use set_parallel_file_name
!
      implicit none
!
      integer(kind=kint) :: nx_2, ny_2
      integer(kind=kint) :: num_ene, num_ene_z
!
      real   (kind=kreal), allocatable  ::  zz(:)

      real   (kind=kreal), allocatable  ::  phys_cxcy(:,:)
      real   (kind=kreal), allocatable  ::  phys_sxcy(:,:)
      real   (kind=kreal), allocatable  ::  phys_cxsy(:,:)
      real   (kind=kreal), allocatable  ::  phys_sxsy(:,:)
      real   (kind=kreal), allocatable  ::  phys_ene(:,:)

      real   (kind=kreal), allocatable  ::  ene_z(:,:)
      real   (kind=kreal), allocatable  ::  ene_xz(:,:)
      real   (kind=kreal), allocatable  ::  ene_yz(:,:)
      real   (kind=kreal), allocatable  ::  ene_xyz(:,:)
!
      real   (kind=kreal), allocatable  ::  horiz_sq(:,:)
!
      character(len=kchara), allocatable :: fft_name(:)
      character(len=kchara), allocatable :: fft_comp(:)
!
      integer(kind=kint) :: ist, ied, iint, num_fft
      integer(kind=kint) :: i1, i2, i3, iii
!
      character(len=kchara) :: spectr_data_name
      character(len=kchara) :: z_stacked_name
      character(len=kchara) :: xz_stacked_name
      character(len=kchara) :: yz_stacked_name
      character(len=kchara) :: xyz_stacked_name
!
      character(len=kchara) :: ene_spec_name
!
      integer(kind=kint), parameter :: spectr_grid_code = 17
      integer(kind=kint), parameter :: spectr_data_code = 19
      integer(kind=kint), parameter :: z_stacked_code =   16
      integer(kind=kint), parameter :: xz_stacked_code =  22
      integer(kind=kint), parameter :: yz_stacked_code =  24
      integer(kind=kint), parameter :: xyz_stacked_code = 28
      integer(kind=kint), parameter :: horiz_ave_code =   18
      integer(kind=kint), parameter :: horiz_rms_code =   29
      integer(kind=kint), parameter :: ene_spec_code =    20

      real(kind = kreal), parameter :: one = 1.0d0, two = 2.0d0
      real(kind = kreal), parameter :: four = 4.0d0
      real(kind = kreal), parameter :: half = one/two, quata = one/four

      character(len=kchara) :: tmpchara
!
      integer(kind=kint) :: ix, iy, iz, j, istep, num_pe
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
      call read_control_data_fft_plane
      write(*,*) 's_set_plane_spectr_file_head'
      call s_set_plane_spectr_file_head(mesh1_file)
      call set_parameters_4_FFT(num_pe, ist, ied, iint)
!
!
      call s_set_numnod_4_plane
!
       nx_2 = nx_all/2+1
       ny_2 = ny_all/2+1
!
       write(*,*) 'nx_2, ny_2', nx_2, ny_2
!
       num_ene_z = nx_2*ny_2
       num_ene   = nx_2*ny_2*nz_all
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
      call add_dat_extension(ene_spec_x_head,  yz_stacked_name)
      call add_dat_extension(ene_spec_y_head,  xz_stacked_name)
      call add_dat_extension(ene_spec_xy_head, xyz_stacked_name)
!
       open (horiz_rms_code,  file=horiz_rms_name,                      &
     &         form='formatted', status ='unknown')
       open (horiz_ave_code,  file=horiz_ave_name,                      &
     &         form='formatted', status ='unknown')
       open (yz_stacked_code,  file=yz_stacked_name,                    &
     &         form='formatted', status ='unknown')
       open (xz_stacked_code,  file=xz_stacked_name,                    &
     &         form='formatted', status ='unknown')
       open (xyz_stacked_code,  file=xyz_stacked_name,                  &
     &         form='formatted', status ='unknown')
!
!    allocate arrays
!
!       write(*,*) num_ene, num_fft
!
       allocate( zz(nz_all) )
!
       allocate( fft_name(num_fft) )
       allocate( fft_comp(num_fft) )
!
       allocate ( phys_cxcy(num_ene,num_fft) )
       allocate ( phys_sxcy(num_ene,num_fft) )
       allocate ( phys_cxsy(num_ene,num_fft) )
       allocate ( phys_sxsy(num_ene,num_fft) )
       allocate ( phys_ene(num_ene,num_fft) )

       allocate ( ene_z(num_ene_z,num_fft) )
       allocate ( ene_xz(ny_2,num_fft) )
       allocate ( ene_yz(nx_2,num_fft) )
       allocate ( ene_xyz(nx_2+ny_2,num_fft) )
!
       allocate ( horiz_sq(nz_all,num_fft) )
!
       phys_cxcy = 0.0d0
       phys_sxcy = 0.0d0
       phys_cxsy = 0.0d0
       phys_sxsy = 0.0d0
       phys_ene = 0.0d0
!
       ene_z   = 0.0d0
       ene_xz  = 0.0d0
       ene_yz  = 0.0d0
       ene_xyz = 0.0d0
       horiz_sq = 0.0d0
!
      open (spectr_grid_code, file=spec_mode_file_name,                 &
     &                       form='formatted', status ='unknown')
!
      read(spectr_grid_code,*)
      do iy = 1, nx_all/2
        do ix = 1, ny_all/2
          do iz = 1, nz_all
            iii = iz*nx_all*ny_all
            read(spectr_grid_code,*) i1, i2, i3, zz(iz)
          end do
        end do
      end do
!
      close(spectr_grid_code)
!
!
!
      do istep = ist, ied, iint
!
!
!
       call s_set_spectr_file_name(istep, spectr_data_name)
       write(*,*) 'file name:', spectr_data_name
       open (spectr_data_code,  file=spectr_data_name,                  &
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
         do iz = 1, nz_all
          iii   = (iy-1)*(nz_all*nx_2) + (ix-1)*nz_all + iz
!          write(*,*) 'tako', j, iy, ix, iz, iii
          read(spectr_data_code,'(1p4e20.11)')                          &
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
!
      do j = 1, num_fft
!
!       kx = ky = 0
!
         do iz = 1, nz_all
!          write(*,*) j, iz
          iii = iz
          phys_ene(iii,j) =        ( phys_cxcy(iii,j)*phys_cxcy(iii,j)  &
     &                            + phys_sxcy(iii,j)*phys_sxcy(iii,j)   &
     &                            + phys_cxsy(iii,j)*phys_cxsy(iii,j)   &
     &                            + phys_sxsy(iii,j)*phys_sxsy(iii,j) )
         end do
!
!       ky = 0
!
        do ix = 2, nx_2 - 1
         do iz = 1, nz_all
          iii = (ix-1)*nz_all + iz
          phys_ene(iii,j) = half * ( phys_cxcy(iii,j)*phys_cxcy(iii,j)  &
     &                            + phys_sxcy(iii,j)*phys_sxcy(iii,j)   &
     &                            + phys_cxsy(iii,j)*phys_cxsy(iii,j)   &
     &                            + phys_sxsy(iii,j)*phys_sxsy(iii,j) )
         end do
        end do
!
!       ky = 0
!
         do iz = 1, nz_all
          iii = (nx_2-1)*nz_all + iz
          phys_ene(iii,j) =       ( phys_cxcy(iii,j)*phys_cxcy(iii,j)   &
     &                            + phys_sxcy(iii,j)*phys_sxcy(iii,j)   &
     &                            + phys_cxsy(iii,j)*phys_cxsy(iii,j)   &
     &                            + phys_sxsy(iii,j)*phys_sxsy(iii,j) )
         end do

       do iy = 2, ny_2-1
         do iz = 1, nz_all
          iii   = (iy-1)*(nz_all*nx_2) + iz
          phys_ene(iii,j) = half * ( phys_cxcy(iii,j)*phys_cxcy(iii,j)  &
     &                            + phys_sxcy(iii,j)*phys_sxcy(iii,j)   &
     &                            + phys_cxsy(iii,j)*phys_cxsy(iii,j)   &
     &                            + phys_sxsy(iii,j)*phys_sxsy(iii,j) )
         end do
        do ix = 2, nx_2 - 1
         do iz = 1, nz_all
          iii   = (iy-1)*(nz_all*nx_2) + (ix-1)*nz_all + iz
          phys_ene(iii,j) = quata * ( phys_cxcy(iii,j)*phys_cxcy(iii,j) &
     &                            + phys_sxcy(iii,j)*phys_sxcy(iii,j)   &
     &                            + phys_cxsy(iii,j)*phys_cxsy(iii,j)   &
     &                            + phys_sxsy(iii,j)*phys_sxsy(iii,j) )
         end do
        end do
         do iz = 1, nz_all
          iii   = (iy-1)*(nz_all*nx_2) + (nx_2-1)*nz_all + iz
          phys_ene(iii,j) = half * ( phys_cxcy(iii,j)*phys_cxcy(iii,j)  &
     &                            + phys_sxcy(iii,j)*phys_sxcy(iii,j)   &
     &                            + phys_cxsy(iii,j)*phys_cxsy(iii,j)   &
     &                            + phys_sxsy(iii,j)*phys_sxsy(iii,j) )
         end do
       end do

         do iz = 1, nz_all
          iii   = (ny_2-1)*(nz_all*nx_2) + iz
          phys_ene(iii,j) =       ( phys_cxcy(iii,j)*phys_cxcy(iii,j)   &
     &                            + phys_sxcy(iii,j)*phys_sxcy(iii,j)   &
     &                            + phys_cxsy(iii,j)*phys_cxsy(iii,j)   &
     &                            + phys_sxsy(iii,j)*phys_sxsy(iii,j) )
         end do
        do ix = 2, nx_2 - 1
         do iz = 1, nz_all
          iii   = (ny_2-1)*(nz_all*nx_2) + (ix-1)*nz_all + iz
          phys_ene(iii,j) = half * ( phys_cxcy(iii,j)*phys_cxcy(iii,j)  &
     &                            + phys_sxcy(iii,j)*phys_sxcy(iii,j)   &
     &                            + phys_cxsy(iii,j)*phys_cxsy(iii,j)   &
     &                            + phys_sxsy(iii,j)*phys_sxsy(iii,j) )
         end do
        end do
         do iz = 1, nz_all
          iii   = (ny_2-1)*(nz_all*nx_2) + (nx_2-1)*nz_all + iz
          phys_ene(iii,j) =       ( phys_cxcy(iii,j)*phys_cxcy(iii,j)   &
     &                            + phys_sxcy(iii,j)*phys_sxcy(iii,j)   &
     &                            + phys_cxsy(iii,j)*phys_cxsy(iii,j)   &
     &                            + phys_sxsy(iii,j)*phys_sxsy(iii,j) )
         end do

      end do
!
!
      ene_z = 0.0d0
      ene_xz = 0.0d0
      ene_yz = 0.0d0
      ene_xyz = 0.0d0
!
      do j = 1, num_fft
       do iy = 1, ny_2
        do ix = 1, nx_2
          i1    = (iy-1)*(nx_2) + ix
          iii   = (iy-1)*(nz_all*nx_2) + (ix-1)*nz_all + 1
          ene_z(i1,j) = ene_z(i1,j)                                     &
     &          + half * phys_ene(iii,j) * ( zz(2)-zz(1) )
         end do
        end do
!
       do iz = 2, nz_all - 1
        do iy = 1, ny_2
         do ix = 1, nx_2
          i1    = (iy-1)*(nx_2) + ix
          iii   = (iy-1)*(nz_all*nx_2) + (ix-1)*nz_all + iz
          ene_z(i1,j) = ene_z(i1,j)                                     &
     &          + phys_ene(iii,j) * ( zz(iz+1)-zz(iz) )
         end do
        end do
       end do
!
        do iy = 1, ny_2
         do ix = 1, nx_2
          i1    = (iy-1)*(nx_2) + ix
          iii   = (iy-1)*(nz_all*nx_2) + (ix-1)*nz_all + nz_all
          ene_z(i1,j) = ene_z(i1,j)                                     &
     &          + half * phys_ene(iii,j) * ( zz(nz_all)-zz(nz_all-1) )
         end do
        end do
!
        do iy = 1, ny_2
         do ix = 1, nx_2
          i1    = (iy-1)*(nx_2) + ix
          ene_z(i1,j) = ene_z(i1,j) / (zz(nz_all) - zz(1))
         end do
        end do
!
       end do
!
!
       do j = 1, num_fft
        do iy = 1, ny_2
         do ix = 1, nx_2
          i1 = (iy-1)*(nx_2) + ix
          ene_xz(iy,j) = ene_xz(iy,j) + ene_z(i1,j)
          ene_yz(ix,j) = ene_yz(ix,j) + ene_z(i1,j)
          ene_xyz(ix+iy-1,j) = ene_xyz(ix+iy-1,j) + ene_z(i1,j)
        end do
       end do
      end do
!
      horiz_sq = 0.0d0
      do j = 1, num_fft
        do iy = 1, ny_2
          do ix = 1, nx_2
            do iz = 1, nz_all
              iii   = (iy-1)*(nz_all*nx_2) + (ix-1)*nz_all + iz
              horiz_sq(iz,j) = horiz_sq(iz,j) + phys_ene(iii,j)
            end do
          end do
        end do
      end do
!
!
      do j = 1, num_fft
        if (   fft_name(j) .eq. fhd_velo                                &
     &    .or. fft_name(j) .eq. fhd_magne                               &
     &    .or. fft_name(j) .eq. fhd_filter_velo                         &
     &    .or. fft_name(j) .eq. fhd_filter_magne                        &
     &     ) then
          do iy = 1, ny_2
            do ix = 1, nx_2
              do iz = 1, nz_all
                iii   = (iy-1)*(nz_all*nx_2) + (ix-1)*nz_all + iz
                phys_ene(iii,j) = half * phys_ene(iii,j)
              end do
            end do
          end do
          do iy = 1, ny_2
            do ix = 1, nx_2
              iii   = (iy-1)*(nx_2) + ix
              ene_z(iii,j) = half * ene_z(iii,j)
             end do
          end do
          do iy = 1, ny_2
            ene_xz(iy,j) = half * ene_xz(iy,j)
          end do
          do ix = 1, ny_2
            ene_yz(ix,j) = half * ene_yz(ix,j)
          end do
          do iii = 1, nx_2+ny_2
            ene_xyz(iii,j) = half * ene_xyz(iii,j)
          end do
          do iz = 1, nz_all
            horiz_sq(iz,j) = half * horiz_sq(iz,j)
          end do
        else
          do iy = 1, ny_2
            do ix = 1, nx_2
              do iz = 1, nz_all
                iii   = (iy-1)*(nz_all*nx_2) + (ix-1)*nz_all + iz
                phys_ene(iii,j) = sqrt(phys_ene(iii,j))
              end do
            end do
          end do
          do iy = 1, ny_2
            do ix = 1, nx_2
              iii   = (iy-1)*(nx_2) + ix
              ene_z(iii,j) =  sqrt(ene_z(iii,j))
            end do
          end do
          do iy = 1, ny_2
            ene_xz(iy,j) = sqrt(ene_xz(iy,j))
          end do
          do ix = 1, ny_2
            ene_yz(ix,j) = sqrt(ene_yz(ix,j))
          end do
          do iii = 1, nx_2+ny_2
            ene_xyz(iii,j) = sqrt(ene_xyz(iii,j))
          end do
        end if
      end do
!
!
       do j = 1, num_fft
        if (   fft_comp(j) .eq. 'vector') then
          do iz = 1, nz_all
            horiz_sq(iz,j) = horiz_sq(iz,j-3) + horiz_sq(iz,j-2)        &
     &                       + horiz_sq(iz,j-1)
          end do
        else if (   fft_comp(j) .eq. 'tensor') then
          do iz = 1, nz_all
            horiz_sq(iz,j) = two*horiz_sq(iz,j-6) + horiz_sq(iz,j-5)    &
     &                      + horiz_sq(iz,j-4) + two*horiz_sq(iz,j-3)   &
     &                      + horiz_sq(iz,j-2) + two*horiz_sq(iz,j-1)
          end do
        end if
       end do
!
!    output data
!
       call s_set_ene_spec_file_name(istep, ene_spec_name)
       call s_set_horiz_ene_file_name(istep, z_stacked_name)
!
       open (ene_spec_code,  file=ene_spec_name,                        &
     &         form='formatted', status ='unknown')
       open (z_stacked_code,  file=z_stacked_name,                      &
     &         form='formatted', status ='unknown')


       if (istep .eq. ist) then
        write(horiz_rms_code,*)   '# number of component'
        write(horiz_ave_code,*)   '# number of component'
        write(xz_stacked_code,*)  '# number of component'
        write(yz_stacked_code,*)  '# number of component'
        write(xyz_stacked_code,*) '# number of component'
        write(horiz_rms_code,*)   num_fft
        write(horiz_ave_code,*)   num_fft
        write(xz_stacked_code,*)  num_fft
        write(yz_stacked_code,*)  num_fft
        write(xyz_stacked_code,*) num_fft
        do j = 1, num_fft
         write(horiz_rms_code,*)                                        &
     &        trim(fft_name(j)), '_', trim(fft_comp(j))
         write(horiz_ave_code,*)                                        &
     &        trim(fft_name(j)), '_', trim(fft_comp(j))
         write(xz_stacked_code,*)                                       &
     &        trim(fft_name(j)), '_', trim(fft_comp(j))
         write(yz_stacked_code,*)                                       &
     &        trim(fft_name(j)), '_', trim(fft_comp(j))
         write(xyz_stacked_code,*)                                      &
     &        trim(fft_name(j)), '_', trim(fft_comp(j))
        end do
       end if
!
       write(z_stacked_code,*) '# number of component'
       write(ene_spec_code,*)  '# number of component'
       write(z_stacked_code,*) num_fft
       write(ene_spec_code,*) num_fft
       do j = 1, num_fft
        write(z_stacked_code,*)                                         &
     &        trim(fft_name(j)), '_', trim(fft_comp(j))
        write(ene_spec_code, *)                                         &
     &        trim(fft_name(j)), ' ', trim(fft_comp(j))
       end do
!
       do iz = 1, nz_all
        write(horiz_ave_code,'(2i16,1p120e20.11)') istep, iz, zz(iz),   &
     &              (phys_cxcy(iz,j),j=1,num_fft)
        write(horiz_rms_code,'(2i16,1p120e20.11)') istep, iz, zz(iz),   &
     &              (horiz_sq(iz,j),j=1,num_fft)
       end do
!
       do iii = 0, nx_2+ny_2-2
        i1 = iii + 1
        write(xyz_stacked_code,'(2i16,1p120e20.11)') istep, iii,        &
     &              (ene_xyz(i1,j),j=1,num_fft)
       end do
!
       do ix = 0, nx_2-1
        i1 = ix+1
        write(yz_stacked_code,'(2i16,1p120e20.11)') istep, ix,          &
     &              (ene_yz(i1,j),j=1,num_fft)
       end do
!
       do iy = 0, ny_2-1
        i1 = iy+1
        write(xz_stacked_code,'(2i16,1p120e20.11)') istep, iy,          &
     &              (ene_xz(i1,j),j=1,num_fft)
       end do
!
       do iy = 0, ny_2-1
        do ix = 0, nx_2-1
          i1    = iy*(nx_2) + ix+1
         write(z_stacked_code,'(2i16,1p120e20.11)') ix, iy,             &
     &          (ene_z(i1,j), j=1,num_fft)
        end do
       end do
!
       do iy = 0, ny_2-1
        do ix = 0, nx_2-1
         do iz = 1, nz_all
          iii   = iy*(nz_all*nx_2) + ix*nz_all + iz
          write(ene_spec_code,'(3i16,1p120e20.11)') ix, iy, iz,         &
     &          (phys_ene(iii,j),j=1,num_fft)
         end do
        end do
       end do
!
       close (ene_spec_code)
       close (z_stacked_code)
!
      end do
!
      close (horiz_rms_code)
      close (horiz_ave_code)
      close (xz_stacked_code)
      close (yz_stacked_code)
!
      stop
      end
