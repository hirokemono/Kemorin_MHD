!
!      program make_physical_data
!
!      constract result data from spectr
!
!      Written by Kemorin
!
      program make_physical_data
!
      use m_precision
!
      use m_constants
      use m_file_format_switch
!
      use t_ctl_data_plane_fft
      use t_size_of_cube
      use t_time_data
      use t_ucd_data
      use t_mesh_data_4_merge
      use t_set_new_spectr
      use t_spectr_4_ispack
      use m_FFT_selector
!
      use count_number_with_overlap
      use set_merged_geometry
      use set_plane_spectr_file_head
      use set_list_4_FFT
      use set_numnod_4_plane
      use inverse_fft_4_plane
      use radial_interpolate
      use read_positions_of_spectr
      use set_merged_udt_2_IO
      use vtk_file_IO
      use ucd_IO_select
!
       implicit none
!
      type(ctl_data_plane_fft), save :: pfft_c1
      type(size_of_cube), save :: c_size1
      type(plane_spectr_by_ispack), save :: plane_fft_wk1
      type(new_plane_spectr), save :: npl_spec1
      type(field_IO_params), save ::  plane_mesh_file, ucd_file_param
      type(ucd_data) :: fft_ucd
      type(time_data), save :: fft_t_IO
      type(merged_mesh), save :: mgd_mesh_pm
      type(second_mesh), save :: sec_mesh_pm
!
      integer(kind=kint ) :: ist, ied, iint
      integer(kind=kint ) ::  istep
!
!  ===========
! . for local 
!  ===========

      integer(kind=kint ) :: i,j
      integer(kind=kint ) :: iz
      integer(kind=kint ) :: i1, nnod, nele

      real   (kind=kreal), dimension(:), allocatable ::  zz
!
      integer(kind=kint ) :: kx_org, ky_org, iz_org
      character(len=kchara) :: tmpchara

! ==============================================
! * get number of  nodes,elements for whole PES
! ==============================================

      write(*,*) ' Dou you prepare folloing data???'
      write(*,*) ' mesh data for new simulation:  mesh/in.PE#'
      write(*,*) ' spectr data:  spectr/spectr.step#.dat'
      write(*,*) ' output data:  outall.step#.file_type'
      write(*,*) ' hit return'
      read(*,*)
!
!  set parameters for FFT
!
      write(*,*) 'read_control_data_fft_plane'
      call read_control_data_fft_plane(pfft_c1)
!
      call s_set_plane_spectr_file_head(pfft_c1, plane_mesh_file)
      call set_parameters_4_FFT(pfft_c1%t_zfft_ctl, pfft_c1%cube_c_fft, &
     &    c_size1, plane_fft_wk1, mgd_mesh_pm%num_pe, ist, ied, iint)
      call set_parameters_data_by_spec                                  &
     &   (pfft_c1%new_p_plt, pfft_c1%cube_c_fft, pfft_c1%cube2nd_cf,    &
     &    c_size1, mgd_mesh_pm%num_pe, kx_org, ky_org, iz_org,          &
     &    npl_spec1%nnod_new_k_org_z, plane_mesh_file, plane_fft_wk1,   &
     &    ucd_file_param)
      call s_set_numnod_4_plane(c_size1, mgd_mesh_pm%merge_tbl)
!
      call alloc_z_compliment_info(c_size1%nz_all, npl_spec1)
!
!    setting for spectr
!
       allocate( zz(0:plane_fft_wk1%iz_max) )
      zz = 0.0d0
!
      call s_read_positions_of_spectr(plane_fft_wk1%iz_max, zz(1))
      call read_number_of_field(ist, plane_fft_wk1)
!
      call alloc_spectr_name(plane_fft_wk1)
      call alloc_spectr_4_io(plane_fft_wk1)
!
!   read mesh data for initial values
!
      plane_mesh_file%iflag_format = id_ascii_file_fmt
      call set_merged_mesh_and_group(plane_mesh_file, mgd_mesh_pm)
!
      allocate(sec_mesh_pm%subdomains_2(sec_mesh_pm%num_pe2) )
!
      call copy_plane_resolution(plane_fft_wk1%iz_max,                  &
     &    mgd_mesh_pm%num_pe, mgd_mesh_pm%subdomain,                    &
     &    mgd_mesh_pm%merged, mgd_mesh_pm%merge_tbl,                    &
     &    sec_mesh_pm%num_pe2, sec_mesh_pm%subdomains_2,                &
     &    sec_mesh_pm%merge_tbl_2, npl_spec1)
!
! allocate arrays for spectr
!
      call alloc_horiz_spectr(plane_fft_wk1)
!
!    read spectral data
!
      call read_spectr_data(ist, plane_fft_wk1)
!
!
!  set data on grid
!
      call set_field_to_z_grid(plane_fft_wk1%num_fft,                   &
     &    plane_fft_wk1%fft_name, plane_fft_wk1%fft_comp,               &
     &    mgd_mesh_pm%merged_fld)
!
!      write(*,*) 'allocate_merged_field_data'
      call alloc_phys_data(mgd_mesh_pm%merged%node%numnod,              &
     &                     mgd_mesh_pm%merged_fld)
!
      npl_spec1%ncomp_nsp = plane_fft_wk1%num_fft
!
!      write(*,*) 'allocate_index_4_trans'
      call alloc_work_array_4_r                                         &
     &    (mgd_mesh_pm%merge_tbl%inter_nod_m, npl_spec1)
!
!      write(*,*) 'alloc_index_4_trans'
      call alloc_index_4_trans(npl_spec1)
!
      do i = 1, plane_fft_wk1%num_fft
        npl_spec1%idx_field(i) = i
      end do
!
!
      do istep = ist, ied, iint
!
!    read spectral data
!
        plane_fft_wk1%kx_max = kx_org
        plane_fft_wk1%ky_max = ky_org
        plane_fft_wk1%iz_max = iz_org
        plane_fft_wk1%num_spectr = kx_org * ky_org * iz_org
        call read_spectr_data(istep, plane_fft_wk1)
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
        call set_new_spectr                                             &
     &     (c_size1%nx_all, c_size1%ny_all, c_size1%nz_all,             &
     &      plane_fft_wk1%kx_max, plane_fft_wk1%ky_max, npl_spec1)
!
!    deallocate old spectram data
!
        call dealloc_spectr_name(plane_fft_wk1)
        call dealloc_horiz_spectr(plane_fft_wk1)
!
!    set new array size for spectr
!
        plane_fft_wk1%kx_max = c_size1%nx_all
        plane_fft_wk1%ky_max = c_size1%ny_all
        plane_fft_wk1%iz_max = c_size1%nz_all
        plane_fft_wk1%num_spectr = mgd_mesh_pm%merge_tbl%inter_nod_m
!
!           write(*,*) 'num_spectr 0', num_spectr
!    allocate new spectr
!
        call alloc_spectr_name(plane_fft_wk1)
        call alloc_horiz_spectr(plane_fft_wk1)
!
!   reset spectr data
!
        call s_inverse_fft_4_plane(iflag_FFTPACK_ONCE,                  &
     &      npl_spec1, c_size1%nx_all, c_size1%ny_all, c_size1%nz_all,  &
     &      plane_fft_wk1%kx_max, plane_fft_wk1%ky_max,                 &
     &      plane_fft_wk1%iz_max, plane_fft_wk1%num_spectr,             &
     &      plane_fft_wk1%num_fft, plane_fft_wk1%wk_pfft,               &
     &      plane_fft_wk1%phys_d)

        call copy_2_inverted_udt(c_size1%nx_all, c_size1%ny_all,        &
     &      plane_fft_wk1%kx_max, plane_fft_wk1%ky_max,                 &
     &      plane_fft_wk1%iz_max, plane_fft_wk1%num_spectr,             &
     &      plane_fft_wk1%num_fft, plane_fft_wk1%wk_pfft,               &
     &      mgd_mesh_pm%merged_fld)
!
!    output data
!
        call link_merged_node_2_ucd_IO                                  &
     &     (mgd_mesh_pm%merged, mgd_mesh_pm%merge_tbl, fft_ucd)
        call link_merged_field_2_udt_IO                                 &
     &     (mgd_mesh_pm%merged_fld, mgd_mesh_pm%merge_tbl, fft_ucd)
!
        ucd_file_param%iflag_format = iflag_udt
        call sel_write_ucd_file                                         &
     &     (0, istep, ucd_file_param, fft_t_IO, fft_ucd)
        call disconnect_ucd_data(fft_ucd)
      end do
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine copy_plane_resolution                                  &
     &         (iz_max, num_pe, subdomain, merged, merge_tbl,           &
     &          num_pe2, subdomains_2, merge_tbl_2, npl_spec)
!
      use t_mesh_data
      use t_merged_geometry_data
!
      integer, intent(in) :: num_pe
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(mesh_geometry), intent(in):: merged
      type(merged_stacks), intent(in) :: merge_tbl
      integer(kind = kint), intent(in) :: iz_max
!
      integer, intent(in) :: num_pe2
      type(mesh_geometry), intent(inout) :: subdomains_2(num_pe2)
      type(merged_stacks), intent(inout) :: merge_tbl_2
!
      type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, num_pe
        subdomains_2(ip)%node%numnod = subdomain(ip)%node%numnod
        subdomains_2(ip)%ele%numele  = subdomain(ip)%ele%numele
        subdomains_2(ip)%node%internal_node                             &
     &                               = subdomain(ip)%node%internal_node
      end do
!
      call copy_subdomain_stacks(merge_tbl, merge_tbl_2)
!
      call alloc_2nd_merged_geometry(num_pe2, subdomains_2)
      call alloc_2nd_merge_table(merge_tbl_2)
!
      do ip = 1, num_pe
        nnod = subdomain(ip)%node%numnod
        nele = subdomain(ip)%ele%numele
        subdomains_2(ip)%node%inod_global(1:nnod)                       &
     &      = subdomain(ip)%node%inod_global(1:nnod)
        subdomains_2(ip)%ele%iele_global(1:nele)                        &
     &      = subdomain(ip)%ele%iele_global(1:nele)
      end do
!
!  check positions in z-direction
!
      do iz = 1, c_size1%nz_all
        i1 = iz * c_size1%nx_all * c_size1%ny_all
        if(merged%node%xx(i1,3) .eq. zz(1) ) then
          npl_spec%iz_1(iz) = 1
          npl_spec%z_1(iz) = 1.0d0
        end if
          do j = 2, iz_max
           if (merged%node%xx(i1,3).gt.zz(j-1)                          &
     &     .and. merged%node%xx(i1,3).le.zz(j)) then
             npl_spec%iz_1(iz) = j
             npl_spec%z_1(iz)  = (merged%node%xx(i1,3) - zz(j-1) )     &
     &                          / ( zz(j) - zz(j-1) )
        end if
       end do
      end do
!
!      do iz = 1, c_size1%nz_all
!       write(*,*) iz, npl_spec%iz_1(iz), npl_spec%z_1(iz)
!      end do
!
!       write(*,*) 'numnod tako', merge_tbl%nnod_merged
!
      end subroutine copy_plane_resolution
!
!------------------------------------------------------------------
!
      subroutine set_field_to_z_grid                                    &
     &         (num_fft, fft_name, fft_comp, merged_fld)
!
      integer(kind = kint), intent(in) :: num_fft
      character(len=kchara), intent(in) :: fft_name(num_fft)
      character(len=kchara), intent(in) :: fft_comp(num_fft)
!
      type(phys_data), intent(inout) :: merged_fld
!
      integer(kind=kint ) :: i, j
      integer(kind=kint ) :: iii, isig
!
      merged_fld%ntot_phys = num_fft
      merged_fld%num_phys = 0
     i = 1
     do
      isig = 0
      do j = 1, 6
       if (fft_name(i).eq.fft_name(i+j)) isig = isig + 1
      end do
!
      if (isig .eq. 6) then
       if(    fft_comp(i  ).eq.'xx' .and. fft_comp(i+1).eq.'xy'         &
     &  .and. fft_comp(i+2).eq.'xz' .and. fft_comp(i+3).eq.'yy'         &
     &  .and. fft_comp(i+4).eq.'yz' .and. fft_comp(i+5).eq.'zz' ) then
            i = i + isig
            merged_fld%num_phys = merged_fld%num_phys + 1
       end if
      else if (isig .eq. 3) then
       if(    fft_comp(i  ).eq.'x' .and. fft_comp(i+1).eq.'y'           &
     &  .and. fft_comp(i+2).eq.'z'  ) then
            i = i + isig
            merged_fld%num_phys = merged_fld%num_phys + 1
       end if
      else
       i = i + 1
       merged_fld%num_phys = merged_fld%num_phys + 1
      end if
      if ( i.gt. num_fft) exit
     end do
!
     write(*,*) 'alloc_phys_name', merged_fld%num_phys
     call alloc_phys_name(merged_fld)
!
     iii = 0
     i = 1
     do
      isig = 0
      do j = 1, 6
       if (fft_name(i).eq.fft_name(i+j)) isig = isig + 1
      end do
      write(*,*) 'isig', isig, i
      if (isig .eq. 6) then
       if(    fft_comp(i  ).eq.'xx' .and. fft_comp(i+1).eq.'xy'         &
     &  .and. fft_comp(i+2).eq.'xz' .and. fft_comp(i+3).eq.'yy'         &
     &  .and. fft_comp(i+4).eq.'yz' .and. fft_comp(i+5).eq.'zz'         &
     &  .and. fft_comp(i+6).eq.'tensor') then
        iii = iii + 1
        merged_fld%num_component(iii) = 6
        merged_fld%phys_name(iii) = fft_name(i)
!
        iii = iii + 1
        merged_fld%num_component(iii) = 1
        write(tmpchara,*) trim( fft_name(i) ), '_norm' 
        merged_fld%phys_name(iii) = tmpchara
!
        i = i + isig + 1
!
        end if
      else if (isig .eq. 3) then
       if(    fft_comp(i  ).eq.'x' .and. fft_comp(i+1).eq.'y'           &
     &  .and. fft_comp(i+2).eq.'z' .and. fft_comp(i+3).eq.'vector' ) then
        iii = iii + 1
        merged_fld%num_component(iii) = 3
        merged_fld%phys_name(iii) = fft_name(i)
!
        iii = iii + 1
        merged_fld%num_component(iii) = 1
        write(tmpchara,*) trim( fft_name(i) ), '_norm' 
        merged_fld%phys_name(iii) = tmpchara
!
        i = i + isig + 1
        end if
       else
        iii = iii + 1
        merged_fld%num_component(iii) = 1
        merged_fld%phys_name(iii) = fft_name(i)
        i = i + 1
       end if
!      write(*,*) i, num_fft
      if ( i.gt. num_fft) exit
     end do
!
      merged_fld%istack_component(0) = 0
      do j = 1, merged_fld%num_phys
        merged_fld%istack_component(j)                                  &
     &           = merged_fld%istack_component(j-1)                     &
     &            + merged_fld%num_component(j)
      end do
!
      end subroutine set_field_to_z_grid
!
!------------------------------------------------------------------
!
      end program make_physical_data
