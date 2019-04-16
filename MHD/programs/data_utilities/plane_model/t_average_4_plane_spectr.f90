!t_average_4_plane_spectr.f90
!      program t_average_4_plane_spectr
!
      program t_average_4_plane_spectr
!
!      Written by H. Matsui on Nov., 2007
!
      use m_precision
!
      use set_numnod_4_plane
      use set_parallel_file_name
!
      use t_mesh_data_4_merge
      use t_size_of_cube
      use m_control_plane_fft
      use set_list_4_FFT
      use set_spectr_file_name
      use set_plane_spectr_file_head
!
      implicit none
!
      type(field_IO_params), save ::  plane_mesh_file
      type(merged_mesh), save :: mgd_mesh_pm
      type(size_of_cube), save :: c_size1
!
      character(len=kchara) :: xz_stacked_name
      character(len=kchara) :: yz_stacked_name
      character(len=kchara) :: xyz_stacked_name
!
      integer(kind=kint), parameter :: xz_stacked_code =  22
      integer(kind=kint), parameter :: yz_stacked_code =  24
      integer(kind=kint), parameter :: xyz_stacked_code = 28
      integer(kind=kint), parameter :: horiz_ave_code =   18
      integer(kind=kint), parameter :: horiz_rms_code =   20
!
      real   (kind=kreal), allocatable  ::  zz(:)
      real   (kind=kreal), allocatable  ::  phys_cxcy(:,:)
      real   (kind=kreal), allocatable  ::  horiz_sq(:,:)
      real   (kind=kreal), allocatable  ::  ene_xz(:,:)
      real   (kind=kreal), allocatable  ::  ene_yz(:,:)
      real   (kind=kreal), allocatable  ::  ene_xyz(:,:)
!
      real   (kind=kreal), allocatable  ::  phys_cxcy_tave(:,:)
      real   (kind=kreal), allocatable  ::  horiz_sq_tave(:,:)
      real   (kind=kreal), allocatable  ::  ene_xz_tave(:,:)
      real   (kind=kreal), allocatable  ::  ene_yz_tave(:,:)
      real   (kind=kreal), allocatable  ::  ene_xyz_tave(:,:)
      character(len=kchara), allocatable :: fft_name(:)
!
      character(len=kchara) :: fname_tmp, tmpchara
      integer(kind=kint) :: ist, ied, iint, num_fft
      integer(kind=kint) :: istep, icou, i_layer
      integer(kind=kint) :: ist_true, ied_true
      integer(kind=kint) :: nx_2, ny_2
      integer :: num_pe
      real   (kind=kreal) :: rtmp
!
      integer(kind=kint) :: nd, nxy, i1, ix, iy, iz, iii, itmp, j
!
      write(*,*) ' Dou you prepare folloing data???'
      write(*,*) ' control data for this routine:  ctl_fft'
      write(*,*) ' spectr data:  horiz_average.dat'
      write(*,*) ' spectr data:  ene_spec_x.dat'
      write(*,*) ' spectr data:  ene_spec_y.dat'
      write(*,*) ' spectr data:  ene_spec_xy.dat'
      write(*,*) ' return key to start'
      read(*,*)
!
      write(*,*) 'imput number of layer (whole domain: -1)'
      read(*,*) i_layer
!
!     read outline of mesh
!
      write(*,*) 'read_control_data_fft_plane'
      call read_control_data_fft_plane
      call s_set_plane_spectr_file_head(plane_mesh_file)
      call set_parameters_4_FFT(c_size1, num_pe, ist, ied, iint)
!
!
      call s_set_numnod_4_plane(c_size1, mgd_mesh_pm%merge_tbl)
!
      nx_2 = c_size1%nx_all/2 + 1
      ny_2 = c_size1%ny_all/2 + 1
!
      if (i_layer .eq. -1) then
        xz_stacked_name =  add_dat_extension(ene_spec_y_head)
        yz_stacked_name =  add_dat_extension(ene_spec_x_head)
        xyz_stacked_name = add_dat_extension(ene_spec_xy_head)
      else
        call set_ene_spec_plane_name(i_layer,                           &
     &      yz_stacked_name, xz_stacked_name, xyz_stacked_name)
      end if
!
      write(*,*) 'yz_stacked_name: ',  trim(yz_stacked_name)
      write(*,*) 'xz_stacked_name: ',  trim(xz_stacked_name)
      write(*,*) 'xyz_stacked_name: ', trim(xyz_stacked_name)
!
      open (yz_stacked_code,  file=yz_stacked_name,                     &
     &         form='formatted', status ='unknown')
      open (xz_stacked_code,  file=xz_stacked_name,                     &
     &         form='formatted', status ='unknown')
      open (xyz_stacked_code,  file=xyz_stacked_name,                   &
     &         form='formatted', status ='unknown')
!
      read(xz_stacked_code,*)
      read(yz_stacked_code,*)
      read(xyz_stacked_code,*)
      read(xz_stacked_code,*)  num_fft
      read(yz_stacked_code,*)  num_fft
      read(xyz_stacked_code,*) num_fft
!
      allocate ( fft_name(num_fft) )
!
      allocate ( ene_xz(ny_2,num_fft) )
      allocate ( ene_yz(nx_2,num_fft) )
      allocate ( ene_xyz(nx_2+ny_2,num_fft) )
!
      allocate ( ene_xz_tave (ny_2,num_fft) )
      allocate ( ene_yz_tave (nx_2,num_fft) )
      allocate ( ene_xyz_tave(nx_2+ny_2,num_fft) )
!
      ene_xz  = 0.0d0
      ene_yz  = 0.0d0
      ene_xyz = 0.0d0
!
      ene_xz_tave  = 0.0d0
      ene_yz_tave  = 0.0d0
      ene_xyz_tave = 0.0d0
!
      do j = 1, num_fft
        read(xz_stacked_code,*)  fft_name(j)
        read(yz_stacked_code,*)  fft_name(j)
        read(xyz_stacked_code,*) fft_name(j)
      end do
!
      if (i_layer .eq. -1) then
!
        write(*,*) 'horiz_ave_name: ',   trim(horiz_ave_name)
        write(*,*) 'horiz_rms_name: ',   trim(horiz_rms_name)
        open (horiz_ave_code,  file=horiz_ave_name,                     &
     &         form='formatted', status ='unknown')
        open (horiz_rms_code,  file=horiz_rms_name,                     &
     &         form='formatted', status ='unknown')
        read(horiz_ave_code,*)
        read(horiz_ave_code,*)   num_fft
        read(horiz_rms_code,*)
        read(horiz_rms_code,*)   itmp
!
        allocate ( zz(c_size1%nz_all) )
        allocate ( phys_cxcy(c_size1%nz_all,num_fft) )
        allocate ( horiz_sq(c_size1%nz_all,num_fft) )
        allocate ( phys_cxcy_tave (c_size1%nz_all,num_fft) )
        allocate ( horiz_sq_tave(c_size1%nz_all,num_fft) )
        phys_cxcy =      0.0d0
        phys_cxcy_tave = 0.0d0
        horiz_sq =       0.0d0
        horiz_sq_tave =  0.0d0
!
        do j = 1, num_fft
          read(horiz_ave_code,*)  fft_name(j)
          read(horiz_rms_code,*)  tmpchara
        end do
      end if
!
!
!
      ist_true = -1
      icou = 0
      do
!
        if (i_layer .eq. -1) then
          do iz = 1, c_size1%nz_all
            read(horiz_ave_code,*,err=99,end=99) istep, itmp, zz(iz),   &
     &              phys_cxcy(iz,1:num_fft)
            read(horiz_rms_code,*,err=99,end=99) itmp, itmp, rtmp,      &
     &              horiz_sq(iz,1:num_fft)
          end do
        end if
!
        do iii = 0, nx_2+ny_2-2
          i1 = iii + 1
          read(xyz_stacked_code,*,err=99,end=99) istep, itmp,           &
     &              ene_xyz(i1,1:num_fft)
        end do
!
        do ix = 0, nx_2-1
          i1 = ix+1
          read(yz_stacked_code,*,err=99,end=99) istep, itmp,            &
     &              ene_yz(i1,1:num_fft)
        end do
!
        do iy = 0, ny_2-1
          i1 = iy+1
          read(xz_stacked_code,*,err=99,end=99) istep, itmp,            &
     &              ene_xz(i1,1:num_fft)
        end do
!
        if (istep .ge. ist) then
          if (ist_true .eq. -1) ist_true = istep
          icou = icou + 1
          do nd = 1, num_fft
            nxy = nx_2+ny_2
            ene_xz_tave(1:ny_2,nd) =      ene_xz_tave(1:ny_2,nd)        &
     &                                   + ene_xz(1:ny_2,nd)
            ene_yz_tave(1:nx_2,nd) =      ene_yz_tave(1:nx_2,nd)        &
     &                                   + ene_yz(1:nx_2,nd)
            ene_xyz_tave(1:nxy,nd) = ene_xyz_tave(1:nxy,nd)             &
     &                                   + ene_xyz(1:nxy,nd)
          end do
!
          if (i_layer .eq. -1) then
            do nd = 1, num_fft
              phys_cxcy_tave(1:c_size1%nz_all,nd)                       &
     &                = phys_cxcy_tave(1:c_size1%nz_all,nd)             &
     &                 + phys_cxcy(1:c_size1%nz_all,nd)
              horiz_sq_tave(1:c_size1%nz_all,nd)                        &
     &                = horiz_sq_tave(1:c_size1%nz_all,nd)              &
     &                 + horiz_sq(1:c_size1%nz_all,nd)
            end do
          end if
!
        end if
!
        if (istep .gt. ied) exit
!
        write(*,*) 'step', istep, ' finished. Count: ', icou
!
      end do
!
      99 continue
      if (i_layer .eq. -1) then
        close(horiz_ave_code)
        close(horiz_rms_code)
      end if
      close(xyz_stacked_code)
      close(yz_stacked_code)
      close(xz_stacked_code)
!
      ied_true = istep
      do nd = 1, num_fft
        nxy = nx_2+ny_2
        ene_xz_tave(1:ny_2,nd) = ene_xz_tave(1:ny_2,nd) / dble(icou)
        ene_yz_tave(1:nx_2,nd) = ene_yz_tave(1:nx_2,nd) / dble(icou)
        ene_xyz_tave(1:nxy,nd) = ene_xyz_tave(1:nxy,nd) / dble(icou)
      end do
!
      if (i_layer .eq. -1) then
        do nd = 1, num_fft
          phys_cxcy_tave(1:c_size1%nz_all,nd)                           &
     &          = phys_cxcy_tave(1:c_size1%nz_all,nd) / dble(icou)
          horiz_sq_tave(1:c_size1%nz_all,nd)                            &
     &          = horiz_sq_tave(1:c_size1%nz_all,nd) / dble(icou)
        end do
      end if
!
!
      if (i_layer .eq. -1) then
        xz_stacked_name = add_dat_extension(t_ene_spec_y_head)
        yz_stacked_name = add_dat_extension(t_ene_spec_x_head)
        xyz_stacked_name = add_dat_extension(t_ene_spec_xy_head)
      else
        fname_tmp = add_int_suffix(i_layer, t_ene_spec_y_head)
        xz_stacked_name = add_dat_extension(fname_tmp)
        fname_tmp = add_int_suffix(i_layer, t_ene_spec_x_head)
        yz_stacked_name = add_dat_extension(fname_tmp)
        fname_tmp =  add_int_suffix(i_layer, t_ene_spec_xy_head)
        xyz_stacked_name = add_dat_extension(fname_tmp)
      end if
!
      open (yz_stacked_code,  file=yz_stacked_name,                     &
     &         form='formatted', status ='unknown')
      open (xz_stacked_code,  file=xz_stacked_name,                     &
     &         form='formatted', status ='unknown')
      open (xyz_stacked_code,  file=xyz_stacked_name,                   &
     &         form='formatted', status ='unknown')
!
      write(xz_stacked_code,*)   '# start and end step'
      write(yz_stacked_code,*)   '# start and end step'
      write(xyz_stacked_code,*)  '# start and end step'
      write(xz_stacked_code,'(2i16)')  ist_true, ied_true
      write(yz_stacked_code,'(2i16)')  ist_true, ied_true
      write(xyz_stacked_code,'(2i16)') ist_true, ied_true
!
      write(xz_stacked_code,*)   '# number of component'
      write(yz_stacked_code,*)   '# number of component'
      write(xyz_stacked_code,*)  '# number of component'
      write(xz_stacked_code,*)  num_fft
      write(yz_stacked_code,*)  num_fft
      write(xyz_stacked_code,*) num_fft
!
      do j = 1, num_fft
        write(xz_stacked_code,*)  trim(fft_name(j))
        write(yz_stacked_code,*)  trim(fft_name(j))
        write(xyz_stacked_code,*) trim(fft_name(j))
      end do
!
      do iii = 0, nx_2+ny_2-2
        i1 = iii + 1
        write(xyz_stacked_code,'(i16,1p120e20.11)') iii,                &
     &            ene_xyz_tave(i1,1:num_fft)
      end do
!
      do ix = 0, nx_2-1
        i1 = ix+1
        write(yz_stacked_code,'(i16,1p120e20.11)') ix,                  &
     &            ene_yz_tave(i1,1:num_fft)
      end do
!
      do iy = 0, ny_2-1
        i1 = iy+1
        write(xz_stacked_code,'(i16,1p120e20.11)') iy,                  &
     &              ene_xz_tave(i1,1:num_fft)
      end do
!
      close(xyz_stacked_code)
      close(yz_stacked_code)
      close(xz_stacked_code)
!
!
      if (i_layer .eq. -1) then
        open (horiz_ave_code,  file=t_horiz_ave_name,                   &
     &         form='formatted', status ='unknown')
        open (horiz_rms_code,  file=t_horiz_rms_name,                   &
     &         form='formatted', status ='unknown')
        write(horiz_ave_code,*)   '# start and end step'
        write(horiz_ave_code,'(2i16)')  ist_true, ied_true
        write(horiz_ave_code,*)   '# number of component'
        write(horiz_ave_code,*)  num_fft
        write(horiz_rms_code,*)   '# start and end step'
        write(horiz_rms_code,'(2i16)')  ist_true, ied_true
        write(horiz_rms_code,*)   '# number of component'
        write(horiz_rms_code,*)  num_fft
!
        do j = 1, num_fft
          write(horiz_ave_code,*)  trim(fft_name(j))
          write(horiz_rms_code,*)  trim(fft_name(j))
        end do
!
        do iz = 1, c_size1%nz_all
          write(horiz_ave_code,'(i16,1p120e20.11)') iz, zz(iz),         &
     &            phys_cxcy_tave(iz,1:num_fft)
          write(horiz_rms_code,'(i16,1p120e20.11)') iz, zz(iz),         &
     &            horiz_sq_tave(iz,1:num_fft)
        end do
        close(horiz_ave_code)
        close(horiz_rms_code)
!
      end if
!
      end program t_average_4_plane_spectr
