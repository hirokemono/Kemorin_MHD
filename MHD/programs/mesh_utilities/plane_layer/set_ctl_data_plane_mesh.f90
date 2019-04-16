!
!      module set_ctl_data_plane_mesh
!
!        programmed by H.Matsui on Aug., 2007
!
!      subroutine s_set_ctl_data_plane_mesh
!
      module set_ctl_data_plane_mesh
!
      use m_precision
      use m_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_ctl_data_plane_mesh
!
      use m_size_of_cube
      use m_cube_position
      use m_grp_data_cub_kemo
      use m_filtering_nod_4_cubmesh
      use m_cube_files_data
      use m_ctl_data_4_plane_model
      use m_ctl_data_4_cub_kemo
      use m_spheric_constants
      use skip_comment_f
!
!
      if (cubmesh_plt%mesh_file_prefix%iflag .gt. 0) then
        mesh_file_header = cubmesh_plt%mesh_file_prefix%charavalue
      else
        mesh_file_header = 'mesh/in'
      end if
!
      if (ffile_cub_ctl%filter_head_ctl%iflag .eq. 1) then
        filter_file_header = ffile_cub_ctl%filter_head_ctl%charavalue
      else
        filter_file_header = 'mesh/filter_node_l'
      end if
!
      if (z_filter_head_ctl%iflag .eq. 1) then
        z_filter_header = z_filter_head_ctl%charavalue
      else
        z_filter_header = 'filter_info'
      end if
!
!
!
      if (plane_size_ctl%iflag .eq. 0) then
        plane_size_ctl%realvalue(1:3) = 1.0d0
      end if
!
      pi = 4.0d0 * atan(1.0d0)
      if (cmp_no_case(unit_len_plane_ctl%charavalue(1),'pi')) then
        c_size1%xsize = pi*plane_size_ctl%realvalue(1)
      else
        c_size1%xsize = plane_size_ctl%realvalue(1)
      end if
!
      if (cmp_no_case(unit_len_plane_ctl%charavalue(2),'pi')) then
        c_size1%ysize = pi*plane_size_ctl%realvalue(2)
      else
        c_size1%ysize = plane_size_ctl%realvalue(2)
      end if
!
      if (cmp_no_case(unit_len_plane_ctl%charavalue(3),'pi')) then
        c_size1%zsize = pi*plane_size_ctl%realvalue(3)
      else
        c_size1%zsize = plane_size_ctl%realvalue(3)
      end if
!
      call set_plane_size(c_size1)
!
      if      (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     label_Chebyshev)) then
        iradi = igrid_Chebyshev
      else if (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     label_half_Cbyv)) then
        iradi = igrid_half_Chebyshev
      else
        iradi = igrid_equidistance
      end if
!
!
      if (nnod_plane_ctl%iflag .gt. 0) then
        c_size1%nx_all = nnod_plane_ctl%intvalue(1)
        c_size1%ny_all = nnod_plane_ctl%intvalue(2)
        c_size1%nz_all = nnod_plane_ctl%intvalue(3)
      else
        c_size1%nx_all = 2
        c_size1%ny_all = 2
        c_size1%nz_all = 2
      end if
!
      if (ndomain_plane_ctl%iflag .gt. 0) then
        c_size1%ndx = ndomain_plane_ctl%intvalue(1)
        c_size1%ndy = ndomain_plane_ctl%intvalue(2)
        c_size1%ndz = ndomain_plane_ctl%intvalue(3)
      else
        c_size1%ndx = 1
        c_size1%ndy = 1
        c_size1%ndz = 1
      end if
!
      if ( ( mod(c_size1%nx_all,c_size1%ndx) .ne. 0 ) .or.              &
     &     ( mod(c_size1%ny_all,c_size1%ndy) .ne. 0 ) .or.              &
     &     ( mod(c_size1%nz_all,c_size1%ndz) .ne. 0 )) then
        stop ' ***** error : illegal input '
      endif
      if (neib .ge. (c_size1%nz_all / c_size1%ndz) ) then
        write(*,*) 'neighbouring should be less than',                  &
    &        (c_size1%nz_all / c_size1%ndz)
        stop ' ***** error : illegal input '
      end if
!
      if (num_of_sleeve_ctl%iflag .gt. 0) then
        ndepth = num_of_sleeve_ctl%intvalue
      else
        ndepth = 1
      end if
!
!
      if (num_z_filter_ctl%iflag .gt. 0) then
        iflag_filter = num_z_filter_ctl%intvalue
      else
        iflag_filter = 0
      end if
!
      iflag_z_filter = 0
!      write(*,*) ' Setting of vertical filter'
!      call skip_comment(character_4_read, l_in)
!      read (character_4_read, * )   iflag_z_filter
!      write(*,*) ' iflag_z_filter    = ',  iflag_z_filter
!
!
!
      if (omitting_value_ctl%iflag .gt. 0) then
        eps_filter = omitting_value_ctl%realvalue
      else
        eps_filter = 0.0d0
      end if
!
!
      write(*,*) 'mesh_file_header:   ', trim(mesh_file_header)
      write(*,*) 'filter_file_header: ', trim(filter_file_header)
      write(*,*) 'z_filter_header:    ', trim(z_filter_header)
!
      write(*,'(a,1p3E25.15e3)') 'size of domain: ',                    &
     &         c_size1%xsize, c_size1%ysize, c_size1%zsize
      write(*,*) 'grid type: ', iradi
!
      write(*,*) 'num. of grid: ',                                      &
     &          c_size1%nx_all, c_size1%ny_all, c_size1%nz_all
      write(*,*) 'num. of domain: ',                                    &
     &          c_size1%ndx, c_size1%ndy, c_size1%ndz
      write(*,*) 'num. of sleeve: ', ndepth
!
      write(*,*) 'num. of filter: ', iflag_filter
      write(*,*) 'omitting parameter: ', eps_filter
!
      end subroutine s_set_ctl_data_plane_mesh
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_data_plane_mesh
