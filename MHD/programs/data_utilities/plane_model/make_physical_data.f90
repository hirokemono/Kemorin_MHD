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
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_size_4_plane
      use m_set_new_spectr
      use m_spectr_4_ispack
      use m_control_plane_fft
      use m_ctl_data_4_plane_model
      use m_ctl_data_2nd_plane
!
      use t_time_data
      use t_ucd_data
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
      use output_newdomain_ucd
      use vtk_file_IO
      use ucd_IO_select
!
       implicit none
!
      type(field_IO_params), save ::  plane_mesh_file, ucd_file_param
      type(ucd_data) :: fft_ucd
      type(time_data), save :: fft_t_IO
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
      call read_control_data_fft_plane
!
      call s_set_plane_spectr_file_head(plane_mesh_file)
      call set_parameters_4_FFT(mgd_mesh1%num_pe, ist, ied, iint)
      call set_parameters_data_by_spec                                  &
     &   (mgd_mesh1%num_pe, kx_org, ky_org, iz_org,                     &
     &    plane_mesh_file, ucd_file_param)
      call s_set_numnod_4_plane
!
      call allocate_z_compliment_info(nz_all)
!
!    setting for spectr
!
       allocate( zz(0:iz_max) )
      zz = 0.0d0
!
      call s_read_positions_of_spectr(iz_max, zz(1))
      call read_number_of_field(ist)
!
      call allocate_spectr_name
      call allocate_spectr_4_io
!
!   read mesh data for initial values
!
      plane_mesh_file%iflag_format = id_ascii_file_fmt
      call set_merged_mesh_and_group(plane_mesh_file)
!
      allocate( subdomains_2(num_pe2) )
!
      call copy_plane_resolution                                        &
     &   (mgd_mesh1%num_pe, mgd_mesh1%subdomain, merge_tbl)
!
!  check positions in z-direction
!
      do iz = 1, nz_all
        i1 = iz*nx_all*ny_all
        if ( mgd_mesh1%merged%node%xx(i1,3) .eq. zz(1) ) then
          iz_1(iz) = 1
          z_1(iz) = 1.0d0
        end if
          do j = 2, iz_max
           if (mgd_mesh1%merged%node%xx(i1,3).gt.zz(j-1)                &
     &     .and. mgd_mesh1%merged%node%xx(i1,3).le.zz(j)) then
         iz_1(iz) = j
         z_1(iz)  = ( mgd_mesh1%merged%node%xx(i1,3) - zz(j-1) )        &
     &             / ( zz(j) - zz(j-1) )
        end if
       end do
      end do
!
!      do iz = 1, nz_all
!       write(*,*) iz, iz_1(iz), z_1(iz)
!      end do
!
!       write(*,*) 'numnod tako', merge_tbl%nnod_merged
!
! allocate arrays for spectr
!
      call allocate_horiz_spectr
!
!    read spectral data
!
      call read_spectr_data(ist)
!
!
!  set data on grid
!
      call set_field_to_z_grid(mgd_mesh1%merged_fld)
!
!      write(*,*) 'allocate_merged_field_data'
      call alloc_phys_data_type                                         &
     &   (mgd_mesh1%merged%node%numnod, mgd_mesh1%merged_fld)
!
      ncomp_nsp = num_fft
!
!      write(*,*) 'allocate_index_4_trans'
      call allocate_work_array_4_r(merge_tbl%inter_nod_m)
!
!      write(*,*) 'allocate_index_4_trans'
      call allocate_index_4_trans
!
      do i = 1, num_fft
       idx_field(i) = i
      end do
!
!
      do istep = ist, ied, iint
!
!    read spectral data
!
        kx_max = kx_org
        ky_max = ky_org
        iz_max = iz_org
        num_spectr = kx_org*ky_org*iz_org
        call read_spectr_data(istep)
!
!     interpolate in radial direction
!
        write(*,*) 's_radial_interpolate'
        call s_radial_interpolate
!
!  set new spectr
!
        call set_new_spectr
!
!    deallocate old spectram data
!
        call deallocate_spectr_name
        call deallocate_horiz_spectr
!
!    set new array size for spectr
!
        kx_max = nx_all
        ky_max = ny_all
        iz_max = nz_all
        num_spectr = merge_tbl%inter_nod_m
!
!           write(*,*) 'num_spectr 0', num_spectr
!    allocate new spectr
!
        call allocate_spectr_name
        call allocate_horiz_spectr
!
!   reset spectr data
!
        call s_inverse_fft_4_plane
        call copy_2_inverted_udt(mgd_mesh1%merged_fld)
!
!    output data
!
        call link_merged_node_2_ucd_IO                                  &
     &     (mgd_mesh1%merged, merge_tbl, fft_ucd)
        call link_merged_field_2_udt_IO                                 &
     &     (mgd_mesh1%merged_fld, merge_tbl, fft_ucd)
!
        ucd_file_param%iflag_format = iflag_udt
        call sel_write_ucd_file                                         &
     &     (izero, istep, ucd_file_param, fft_t_IO, fft_ucd)
        call disconnect_ucd_data(fft_ucd)
      end do
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine copy_plane_resolution(num_pe, subdomain, merge_tbl)
!
      use m_2nd_geometry_4_merge
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in) :: num_pe
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(merged_stacks), intent(in) :: merge_tbl
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
      call allocate_2nd_merged_geometry
      call allocate_2nd_merge_table
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
      end subroutine copy_plane_resolution
!
!------------------------------------------------------------------
!
      subroutine set_field_to_z_grid(merged_fld)
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
     write(*,*) 'alloc_phys_name_type', merged_fld%num_phys
     call alloc_phys_name_type(merged_fld)
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
