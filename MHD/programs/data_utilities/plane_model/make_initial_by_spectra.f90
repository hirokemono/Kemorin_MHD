!
!      program make_initial_by_spectra
!
!    constract new initial data from simulation results 
!     By H. Matsui
!
!
      program make_initial_by_spectra
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_time_data
      use t_mesh_data_4_merge
      use t_size_of_cube
      use t_ctl_data_plane_fft
      use t_setting_4_ini
      use t_set_new_spectr
      use t_spectr_4_ispack
!
      use m_file_format_switch
      use m_FFT_selector
      use count_number_with_overlap
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
      character (len = kchara), parameter                               &
     &                         :: control_file_name = 'ctl_fft'
!
      type(ctl_data_plane_fft), save :: pfft_c1
      type(plane_spectr_by_ispack), save :: plane_fft_wk1
      type(new_plane_spectr), save :: npl_spec1
      type(field_IO_params), save ::  plane_mesh_file
      type(merged_mesh), save :: mgd_mesh_pm
      type(size_of_cube), save :: c_size1
!
      integer(kind=kint ) ::  istep_udt, n_comp, i_time_step
!
!  ===========
! . for local 
!  ===========

      integer(kind=kint ) :: ip, inod
      integer(kind=kint ) :: i, j
      integer(kind=kint ) :: i1
      integer(kind=kint ) :: ist, ied
      integer(kind=kint ) :: ifactor_rst, ifactor_step, istep_rst
!
      integer(kind=kint ) :: kx_org, ky_org, iz_org, nfft_org
      integer(kind=kint ) :: kx_new, ky_new, iz_new, nfft_new
!
      real   (kind=kreal) ::  dt_init, t_init
      real   (kind=kreal), dimension(:), allocatable ::  zz
!
      type(time_data), save :: plane_t_IO
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
      call read_control_data_fft_plane(control_file_name, pfft_c1)
      if(pfft_c1%i_fft_plane_ctl .ne. 1) then
        call calypso_MPI_abort(pfft_c1%i_fft_plane_ctl,                 &
     &                         trim(control_file_name))
      end if
!
      call set_para_plane_spectr_file_def(pfft_c1, plane_mesh_file)
      call set_parameters_rst_by_spec                                   &
     &   (pfft_c1%new_p_plt, pfft_c1%t_zfft_ctl, pfft_c1%cube_c_fft,    &
     &    pfft_c1%cube2nd_cf, c_size1, mgd_mesh_pm%num_pe,              &
     &    ist, ied, ifactor_step, ifactor_rst, dt_init, t_init,         &
     &    kx_org, ky_org, iz_org, npl_spec1%nnod_new_k_org_z,           &
     &    plane_mesh_file, plane_fft_wk1)
!
!     read outline of mesh
!
      call s_set_numnod_4_plane(c_size1, mgd_mesh_pm%merge_tbl)
!
      call alloc_z_compliment_info(c_size1%nz_all, npl_spec1)
!
!    setting for initial values
!
      call set_initial_components(mgd_mesh_pm%merged_fld)
!
      call read_size_of_spectr(plane_fft_wk1)
!
      allocate( zz(0:plane_fft_wk1%iz_max) )
      zz = 0.0d0
!
      call s_read_positions_of_spectr(plane_fft_wk1%iz_max, zz(1))
      istep_udt = ist / ifactor_step
      call read_number_of_field(istep_udt, plane_fft_wk1)
      nfft_org = plane_fft_wk1%num_fft
!
      call alloc_spectr_name(plane_fft_wk1)
      call alloc_spectr_4_io(plane_fft_wk1)
!
!   read mesh data for initial values
!
      plane_mesh_file%iflag_format = id_ascii_file_fmt
      call set_merged_mesh_and_group(plane_mesh_file, mgd_mesh_pm)
!
      write(*,*) 'allocate_rst_by_plane_sp'
      call allocate_rst_by_plane_sp(mgd_mesh_pm%merge_tbl%nnod_max,     &
     &    mgd_mesh_pm%merged_fld%ntot_phys)
!
!  check positions in z-direction
!
     call check_plane_horiz_position                                    &
    &   (mgd_mesh_pm%merged, mgd_mesh_pm%merged_fld,                    &
    &    plane_fft_wk1%iz_max, plane_fft_wk1%num_fft,                   &
    &    plane_fft_wk1%fft_name, plane_fft_wk1%fft_comp, npl_spec1)
!
      kx_new = c_size1%nx_all
      ky_new = c_size1%ny_all
      iz_new = c_size1%nz_all
      nfft_new =   mgd_mesh_pm%merged_fld%ntot_phys
      plane_fft_wk1%num_spectr = mgd_mesh_pm%merge_tbl%inter_nod_m
!
      plane_fft_wk1%kx_max = kx_new
      plane_fft_wk1%ky_max = ky_new
      plane_fft_wk1%iz_max = iz_new
      plane_fft_wk1%num_fft = nfft_new
!
!
!
      call alloc_horiz_spectr(plane_fft_wk1)
      call alloc_work_array_4_r                                         &
    &    (mgd_mesh_pm%merge_tbl%inter_nod_m, npl_spec1)
!
!      do iz = 1, c_size1%nz_all
!       write(*,*) iz, npl_spec%iz_1(iz), npl_spec%z_1(iz)
!      end do
!
!       write(*,*) 'numnod tako', mgd_mesh_pm%merge_tbl%nnod_merged
!
!    start loop for snap shots
!
      do i_time_step = ist, ied, ifactor_step
!
        istep_udt = i_time_step / ifactor_step
        istep_rst = i_time_step / ifactor_rst
        plane_t_IO%i_time_step = i_time_step
        plane_t_IO%time = t_init + dble(i_time_step-ist) * dt_init
        plane_t_IO%dt =  dt_init
!
!    read spectral data
!
        plane_fft_wk1%kx_max = kx_org
        plane_fft_wk1%ky_max = ky_org
        plane_fft_wk1%iz_max = iz_org
        plane_fft_wk1%num_spectr = kx_org*ky_org*iz_org
        plane_fft_wk1%num_fft = nfft_org
!
        call read_spectr_data(istep_udt, plane_fft_wk1)
!
!     interpolate in radial direction
!
        write(*,*) 's_radial_interpolate'
        call s_radial_interpolate(c_size1, plane_fft_wk1%kx_max,        &
     &      plane_fft_wk1%ky_max, plane_fft_wk1%iz_max,                 &
     &      plane_fft_wk1%num_fft, plane_fft_wk1%num_io,                &
     &      plane_fft_wk1%num_spectr, plane_fft_wk1%phys_io,            &
     &      npl_spec1%z_1, npl_spec1%iz_1, npl_spec1%nnod_new_k_org_z,  &
     &      npl_spec1%ncomp_nsp, npl_spec1%idx_field,                   &
     &      npl_spec1%work_array)
!
!  set new spectr
!
        write(*,*) 'set_new_spectr'
        call set_new_spectr                                             &
     &     (c_size1%nx_all, c_size1%ny_all, c_size1%nz_all,             &
     &      plane_fft_wk1%kx_max, plane_fft_wk1%ky_max, npl_spec1)
!
!    deallocate old spectram data
!
!      write(*,*) 'deallocate_spectr_name'
!      call dealloc_spectr_name(plane_fft_wk11)
!
!    set new array size for spectr
!
       plane_fft_wk1%kx_max = kx_new
       plane_fft_wk1%ky_max = ky_new
       plane_fft_wk1%iz_max = iz_new
       plane_fft_wk1%num_spectr = mgd_mesh_pm%merge_tbl%inter_nod_m
       plane_fft_wk1%num_fft = nfft_new
!
           write(*,*) 'num_spectr 0', plane_fft_wk1%num_spectr
!    allocate new spectr
!
!      call allocate_spectr_name
!
        call s_inverse_fft_4_plane(iflag_FFTPACK_ONCE,                  &
     &      npl_spec1, c_size1%nx_all, c_size1%ny_all, c_size1%nz_all,  &
     &      plane_fft_wk1%kx_max, plane_fft_wk1%ky_max,                 &
     &      plane_fft_wk1%iz_max, plane_fft_wk1%num_spectr,             &
     &      plane_fft_wk1%num_fft, plane_fft_wk1%wk_pfft,               &
     &      plane_fft_wk1%phys_d)
        call copy_2_inverted_data                                       &
     &     (c_size1%nx_all, c_size1%ny_all, c_size1%nz_all,             &
     &      plane_fft_wk1%kx_max, plane_fft_wk1%ky_max,                 &
     &      plane_fft_wk1%iz_max, plane_fft_wk1%num_spectr,             &
     &      plane_fft_wk1%num_fft, plane_fft_wk1%wk_pfft,               &
     &      plane_fft_wk1%phys_d)
!
!   read mesh data
!
!
! ========================
! * PES loops 
! ========================
!
        call plane_nnod_stack_4_IO                                      &
     &     (mgd_mesh_pm%num_pe, mgd_mesh_pm%subdomain)
!
        do ip =1, mgd_mesh_pm%num_pe
!
          do j = 1, plane_fft_wk1%num_fft
            do i = 1, mgd_mesh_pm%subdomain(ip)%node%numnod
              inod = int(mgd_mesh_pm%subdomain(ip)%node%inod_global(i), &
     &              KIND(inod))
              if (inod .le. mgd_mesh_pm%merge_tbl%inter_nod_m) then
                i1 = (j-1) * plane_fft_wk1%num_spectr + inod
                rst_from_sp(i,j) = plane_fft_wk1%phys_d(i1)
              else
              rst_from_sp(i,j) = 0.0d0
            end if
          end do
        end do
!
        call s_write_restart_by_spectr(ip, mgd_mesh_pm%num_pe,          &
       &     mgd_mesh_pm%subdomain(ip)%node%numnod,                     &
       &     mgd_mesh_pm%merged_fld, plane_t_IO)
!
!   deallocate arrays
!
       end do
      end do
!
      call calypso_MPI_finalize
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine check_plane_horiz_position(merged, merged_fld,         &
     &          iz_max, num_fft, fft_name, fft_comp, npl_spec)
!
      type(mesh_geometry), intent(in) :: merged
      type(phys_data), intent(in) :: merged_fld
!
      integer(kind = kint), intent(in) :: iz_max
      integer(kind = kint), intent(in) :: num_fft
      character(len=kchara), intent(in) :: fft_name(num_fft)
      character(len=kchara), intent(in) :: fft_comp(num_fft)
!
      type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint ) :: i1, iz
      integer(kind=kint ) :: i, j
      integer(kind=kint ) :: icomp
      integer(kind=kint ) :: iflag
!
!
      do iz = 1, c_size1%nz_all
       i1 = iz * c_size1%nx_all * c_size1%ny_all
       if(merged%node%xx(i1,3) .eq. zz(1)) then
        npl_spec%iz_1(iz) = 1
        npl_spec%z_1(iz) = 1.0d0
       end if
       do j = 2, iz_max
        if (merged%node%xx(i1,3).gt.zz(j-1)                             &
     &       .and. merged%node%xx(i1,3).le.zz(j)) then
         npl_spec%iz_1(iz) = j
         npl_spec%z_1(iz)  = (merged%node%xx(i1,3) - zz(j-1))           &
     &             / ( zz(j) - zz(j-1) )
        end if
       end do
      end do
!
      call read_spectr_data(istep_udt, plane_fft_wk1)
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
      npl_spec%ncomp_nsp = merged_fld%ntot_phys
      call alloc_index_4_trans(npl_spec)
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
                npl_spec%idx_field(icomp) = j
              else if (fft_comp(j) .eq.'y' .or. fft_comp(j) .eq.'Y') then
                npl_spec%idx_field(icomp+1) = j
              else if (fft_comp(j) .eq.'z' .or. fft_comp(j) .eq.'Z') then
                npl_spec%idx_field(icomp+2) = j
              end if
            else
              npl_spec%idx_field(icomp) = j
            end if
          end if
        end do
      end do
!
      end subroutine check_plane_horiz_position
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_para_plane_spectr_file_def(pfft_c, mesh_file)
!
      use m_default_file_prefix
      use t_ctl_data_plane_fft
      use set_spectr_file_name
      use set_parallel_file_name
      use set_control_platform_item
      use set_control_platform_data
!
      type(ctl_data_plane_fft), intent(in) :: pfft_c
      type(field_IO_params), intent(inout) :: mesh_file
!
      character(len = kchara) :: tmpchara
!
!
      call set_ctl_parallel_file_w_def(def_mesh_file_head,              &
     &    pfft_c%new_p_plt%mesh_file_prefix,                            &
     &    pfft_c%new_p_plt%mesh_file_fmt_ctl, mesh_file)
!
      if (pfft_c%new_p_plt%field_file_prefix%iflag .gt. 0) then
        plane_udt_header                                                &
     &       = pfft_c%new_p_plt%field_file_prefix%charavalue
      end if
!
!
      if (pfft_c%plane_spectr_mode_head_ctl%iflag .gt. 0) then
        tmpchara = pfft_c%plane_spectr_mode_head_ctl%charavalue
        spec_mode_file_name = add_dat_extension(tmpchara)
      else
        spec_mode_file_name = spec_mode_def_name
      end if
!
      if (pfft_c%plane_spectr_data_head_ctl%iflag .gt. 0) then
        spec_header = pfft_c%plane_spectr_data_head_ctl%charavalue
      else
        spec_header = spec_def_header
      end if
!
      if (pfft_c%plane_spectr_ene_head_ctl%iflag .gt. 0) then
        ene_header = pfft_c%plane_spectr_ene_head_ctl%charavalue
      else
        ene_header = ene_spec_def_header
      end if
!
      if(pfft_c%plane_spectr_h_ene_head_ctl%iflag .gt. 0) then
        ene_h_header = pfft_c%plane_spectr_h_ene_head_ctl%charavalue
      else
        ene_h_header = ene_h_spec_def_header
      end if
!
      end subroutine set_para_plane_spectr_file_def
!
! -----------------------------------------------------------------------
!
      end program make_initial_by_spectra
