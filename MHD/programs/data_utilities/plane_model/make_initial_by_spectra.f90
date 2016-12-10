!
!      program make_initial_by_spectra
!
      program make_initial_by_spectra
!
!    constract new initial data from simulation results 
!     By H. Matsui
!
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_file_format_switch
      use m_phys_labels
      use m_geometry_data_4_merge
      use m_size_4_plane
      use m_setting_4_ini
      use m_set_new_spectr
      use m_spectr_4_ispack
      use m_time_data_IO
      use m_control_plane_fft
      use count_number_with_overlap
      use set_plane_spectr_file_head
      use set_list_4_FFT
      use set_merged_geometry
      use set_numnod_4_plane
      use inverse_fft_4_plane
      use radial_interpolate
      use read_positions_of_spectr
      use write_restart_by_spectr
      use set_restart_data
!
      implicit    none
!
!
      type(field_IO_params), save ::  plane_mesh_file
!
      integer(kind=kint ) ::  istep_udt, n_comp, i_time_step
!
!  ===========
! . for local 
!  ===========

      integer(kind=kint ) :: ip, inod
      integer(kind=kint ) :: i, j, iz
      integer(kind=kint ) :: icomp
      integer(kind=kint ) :: i1
      integer(kind=kint ) :: iflag
      integer(kind=kint ) :: ist, ied
      integer(kind=kint ) :: ifactor_rst, ifactor_step, istep_rst
!
      integer(kind=kint ) :: kx_org, ky_org, iz_org, nfft_org
      integer(kind=kint ) :: kx_new, ky_new, iz_new, nfft_new
!
      real   (kind=kreal) ::  dt_init, t_init
      real   (kind=kreal), dimension(:), allocatable ::  zz
!
! ==============================================
! * get number of  nodes,elements for whole PES
! ==============================================
!
      call calypso_MPI_init
!

      write(*,*) ' Dou you prepare folloing data???'
      write(*,*) ' mesh data for new simulation:  mesh/in.PE#'
      write(*,*) ' plane information data:  mesh/domainsize.dat'
      write(*,*) ' spectr data:  spectr/spectr.step#.dat'
      write(*,*) ' directory for initial value: rst_new/'
      write(*,*) ' hit return'
      read(*,*)
!
!  set parameters for FFT
!
      write(*,*) 'read_control_data_fft_plane'
      call read_control_data_fft_plane
!
      call s_set_plane_spectr_file_head(plane_mesh_file)
      call set_parameters_rst_by_spec(num_pe, ist, ied,                 &
     &          ifactor_step, ifactor_rst, dt_init, t_init,             &
     &          kx_org, ky_org, iz_org, plane_mesh_file)
!
!     read outline of mesh
!
      call s_set_numnod_4_plane
!
      call allocate_z_compliment_info(nz_all)
!
!    setting for initial values
!
      call set_initial_components
!
      call read_size_of_spectr
!
      allocate( zz(0:iz_max) )
      zz = 0.0d0
!
      call s_read_positions_of_spectr(iz_max, zz(1))
      istep_udt = ist / ifactor_step
      call read_number_of_field(istep_udt)
      nfft_org = num_fft
!
      call allocate_spectr_name
      call allocate_spectr_4_io
!
!   read mesh data for initial values
!
      plane_mesh_file%iflag_format = id_ascii_file_fmt
      call set_merged_mesh_and_group(plane_mesh_file)
!
      write(*,*) 'allocate_rst_by_plane_sp'
      call allocate_rst_by_plane_sp(merge_tbl%nnod_max,                 &
     &    merged_fld%ntot_phys)
!
!  check positions in z-direction
!
      do iz = 1, nz_all
       i1 = iz*nx_all*ny_all
       if ( merged%node%xx(i1,3) .eq. zz(1) ) then
        iz_1(iz) = 1
        z_1(iz) = 1.0d0
       end if
       do j = 2, iz_max
        if (merged%node%xx(i1,3).gt.zz(j-1)                             &
     &       .and. merged%node%xx(i1,3).le.zz(j)) then
         iz_1(iz) = j
         z_1(iz)  = ( merged%node%xx(i1,3) - zz(j-1) )                  &
     &             / ( zz(j) - zz(j-1) )
        end if
       end do
      end do
!
      call read_spectr_data(istep_udt)
!
!   check components of spectr
!
      do i = 1, merged_fld%num_phys
       iflag = 0
       call set_num_comps_4_rst(merged_fld%phys_name(i), n_comp)
       if ( n_comp .eq. 3) then
         do j = 1, num_fft
           if ( merged_fld%phys_name(i) .eq. fft_name(j) ) then
             if( fft_comp(j).eq.'x' .or. fft_comp(j).eq.'X') then
                iflag = iflag + 1
              else if( fft_comp(j).eq.'y' .or. fft_comp(j).eq.'y') then
                iflag = iflag + 2
              else if( fft_comp(j).eq.'z' .or. fft_comp(j).eq.'Z') then
               iflag = iflag + 4
            end if
          end if
        end do
        if (iflag.ne.7) then
         write(*,*) 'there is no component of ',                        &
     &         merged_fld%phys_name(i),'flag:', iflag
         stop
        end if
       else
        do j = 1, num_fft
         if ( merged_fld%phys_name(i) .eq. fft_name(j) ) then
          iflag = iflag + 1
         end if
        end do
        if (iflag.ne.1) then
         write(*,*) 'there is no component for ',                       &
     &              merged_fld%phys_name(i),'flag:', iflag
         stop
        end if
       end if
      end do
!
!  set restart data
!
      ncomp_nsp = merged_fld%ntot_phys
      call allocate_index_4_trans
!
!
!  set index for transfer
!
      do i = 1, merged_fld%num_phys
        icomp = merged_fld%istack_component(i-1) + 1
        do j = 1, num_fft
          if (merged_fld%phys_name(i) .eq. fft_name(j)) then
            if (merged_fld%num_component(i) .eq. 3) then
              if (fft_comp(j) .eq.'x' .or. fft_comp(j) .eq.'X') then
                idx_field(icomp) = j
              else if (fft_comp(j) .eq.'y' .or. fft_comp(j) .eq.'Y') then
                idx_field(icomp+1) = j
              else if (fft_comp(j) .eq.'z' .or. fft_comp(j) .eq.'Z') then
                idx_field(icomp+2) = j
              end if
            else
              idx_field(icomp) = j
            end if
          end if
        end do
      end do
!
      kx_new = nx_all
      ky_new = ny_all
      iz_new = nz_all
      num_spectr = merge_tbl%inter_nod_m
      nfft_new =   merged_fld%ntot_phys
!
      kx_max = kx_new
      ky_max = ky_new
      iz_max = iz_new
      num_fft = nfft_new
!
!
!
      call allocate_horiz_spectr
!
      call allocate_work_array_4_r(merge_tbl%inter_nod_m)
!
!      do iz = 1, nz_all
!       write(*,*) iz, iz_1(iz), z_1(iz)
!      end do
!
!       write(*,*) 'numnod tako', merge_tbl%nnod_merged
!
!    start loop for snap shots
!
      do i_time_step = ist, ied, ifactor_step
!
        istep_udt = i_time_step / ifactor_step
        istep_rst = i_time_step / ifactor_rst
        t1_IO%i_time_step_IO = i_time_step
        t1_IO%time_IO =    t_init + dble(i_time_step-ist) * dt_init
        t1_IO%delta_t_IO = dt_init
!
!    read spectral data
!
        kx_max = kx_org
        ky_max = ky_org
        iz_max = iz_org
        num_spectr = kx_org*ky_org*iz_org
        num_fft = nfft_org
!
        call read_spectr_data(istep_udt)
!
!     interpolate in radial direction
!
        write(*,*) 's_radial_interpolate'
        call s_radial_interpolate
!
!  set new spectr
!
        write(*,*) 'set_new_spectr'
        call set_new_spectr
!
!    deallocate old spectram data
!
!      write(*,*) 'deallocate_spectr_name'
!      call deallocate_spectr_name
!
!    set new array size for spectr
!
       kx_max = kx_new
       ky_max = ky_new
       iz_max = iz_new
       num_spectr = merge_tbl%inter_nod_m
       num_fft = nfft_new
!
           write(*,*) 'num_spectr 0', num_spectr
!    allocate new spectr
!
!      call allocate_spectr_name
!
        call s_inverse_fft_4_plane
        call copy_2_inverted_data
!
!   read mesh data
!
!
! ========================
! * PES loops 
! ========================
!
        call plane_nnod_stack_4_IO
!
        do ip =1, num_pe
!
!        write(*,*) 'numnod', merge_tbl%nnod_merged
!        write(*,*) 'internal_node', merge_tbl%inter_nod_m
!         write(*,*) 'num_spectr', num_spectr
!
          do j = 1, num_fft
            do i = 1, subdomain(ip)%node%numnod
              inod = int(subdomain(ip)%node%inod_global(i))
              if (inod .le. merge_tbl%inter_nod_m) then
                i1 = (j-1)*num_spectr + inod
                rst_from_sp(i,j) = phys_d(i1)
              else
              rst_from_sp(i,j) = 0.0d0
            end if
          end do
        end do
!
        call s_write_restart_by_spectr(ip, subdomain(ip)%node%numnod)
!
!   deallocate arrays
!
       end do
      end do
!
      call calypso_MPI_finalize
!
      end program make_initial_by_spectra
