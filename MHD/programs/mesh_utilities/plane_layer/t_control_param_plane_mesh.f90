!t_control_param_plane_mesh.f90
!      module t_control_param_plane_mesh
!
!        programmed by H.Matsui on Aug., 2007
!
!!      subroutine s_set_ctl_data_plane_mesh(c_size)
!!        type(size_of_cube), intent(inout) :: c_size
!
      module t_control_param_plane_mesh
!
      use m_precision
      use m_constants
!
      implicit  none
!
      type ctl_param_plane_mesh
        integer(kind = kint) :: iflag_filter = -1

        integer(kind = kint) :: iflag_ztype
      end type ctl_param_plane_mesh
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_ctl_data_plane_mesh(cube_p, c_size)
!
      use t_size_of_cube
      use m_grp_data_cub_kemo
      use m_filtering_nod_4_cubmesh
      use m_cube_files_data
      use m_ctl_data_4_plane_model
      use m_ctl_data_4_cub_kemo
      use m_spheric_constants
      use skip_comment_f
!
      type(size_of_cube), intent(inout) :: c_size
      type(ctl_param_plane_mesh), intent(inout) :: cube_p
!
      real(kind = kreal) :: pi
!
!
      pi = four * atan(one)
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
      if (cmp_no_case(unit_len_plane_ctl%charavalue(1),'pi')) then
        c_size%xsize = pi*plane_size_ctl%realvalue(1)
      else
        c_size%xsize = plane_size_ctl%realvalue(1)
      end if
!
      if (cmp_no_case(unit_len_plane_ctl%charavalue(2),'pi')) then
        c_size%ysize = pi*plane_size_ctl%realvalue(2)
      else
        c_size%ysize = plane_size_ctl%realvalue(2)
      end if
!
      if (cmp_no_case(unit_len_plane_ctl%charavalue(3),'pi')) then
        c_size%zsize = pi*plane_size_ctl%realvalue(3)
      else
        c_size%zsize = plane_size_ctl%realvalue(3)
      end if
!
      call set_plane_size(c_size)
!
      if      (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     label_Chebyshev)) then
        cube_p%iflag_ztype = igrid_Chebyshev
      else if (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     label_half_Cbyv)) then
        cube_p%iflag_ztype = igrid_half_Chebyshev
      else
        cube_p%iflag_ztype = igrid_equidistance
      end if
!
!
      if (nnod_plane_ctl%iflag .gt. 0) then
        c_size%nx_all = nnod_plane_ctl%intvalue(1)
        c_size%ny_all = nnod_plane_ctl%intvalue(2)
        c_size%nz_all = nnod_plane_ctl%intvalue(3)
      else
        c_size%nx_all = 2
        c_size%ny_all = 2
        c_size%nz_all = 2
      end if
!
      if (ndomain_plane_ctl%iflag .gt. 0) then
        c_size%ndx = ndomain_plane_ctl%intvalue(1)
        c_size%ndy = ndomain_plane_ctl%intvalue(2)
        c_size%ndz = ndomain_plane_ctl%intvalue(3)
      else
        c_size%ndx = 1
        c_size%ndy = 1
        c_size%ndz = 1
      end if
!
      if ( ( mod(c_size%nx_all,c_size%ndx) .ne. 0 ) .or.                &
     &     ( mod(c_size%ny_all,c_size%ndy) .ne. 0 ) .or.                &
     &     ( mod(c_size%nz_all,c_size%ndz) .ne. 0 )) then
        stop ' ***** error : illegal input '
      endif
      if (neib .ge. (c_size%nz_all / c_size%ndz) ) then
        write(*,*) 'neighbouring should be less than',                  &
    &        (c_size%nz_all / c_size%ndz)
        stop ' ***** error : illegal input '
      end if
!
      if (num_of_sleeve_ctl%iflag .gt. 0) then
        c_size%ndepth = num_of_sleeve_ctl%intvalue
      else
        c_size%ndepth = 1
      end if
!
!
      if (num_z_filter_ctl%iflag .gt. 0) then
        cube_p%iflag_filter = num_z_filter_ctl%intvalue
      else
        cube_p%iflag_filter = 0
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
     &         c_size%xsize, c_size%ysize, c_size%zsize
      write(*,*) 'grid type: ', cube_p%iflag_ztype
!
      write(*,*) 'num. of grid: ',                                      &
     &          c_size%nx_all, c_size%ny_all, c_size%nz_all
      write(*,*) 'num. of domain: ',                                    &
     &          c_size%ndx, c_size%ndy, c_size%ndz
      write(*,*) 'num. of sleeve: ', c_size%ndepth
!
      write(*,*) 'num. of filter: ', cube_p%iflag_filter
      write(*,*) 'omitting parameter: ', eps_filter
!
      end subroutine s_set_ctl_data_plane_mesh
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_plane_mesh
