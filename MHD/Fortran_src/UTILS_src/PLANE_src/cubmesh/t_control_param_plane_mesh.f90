!t_control_param_plane_mesh.f90
!      module t_control_param_plane_mesh
!
!        programmed by H.Matsui on Aug., 2007
!
!!      subroutine s_set_ctl_data_plane_mesh(cube_c, cube_p, c_size)
!!        type(ctl_data_4_plane_model), intent(in) :: cube_c
!!        type(size_of_cube), intent(inout) :: c_size
!!        type(ctl_param_plane_mesh), intent(inout) :: cube_p
!
      module t_control_param_plane_mesh
!
      use m_precision
      use m_constants
!
      implicit  none
!
      type ctl_param_plane_mesh
        character(len=kchara) :: mesh_file_prefix
        character(len=kchara) :: filter_file_prefix
        character(len=kchara) :: z_filter_prefix

        integer(kind = kint) :: iflag_filter = -1

        integer(kind = kint) :: iflag_ztype
!
        integer (kind = kint) :: iflag_z_filter
        real(kind = kreal) :: eps_filter
      end type ctl_param_plane_mesh
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_ctl_data_plane_mesh(cubmesh_c, cube_p, c_size)
!
      use t_size_of_cube
      use t_ctl_data_4_plane_model
      use t_ctl_data_4_cub_kemo
      use m_spheric_constants
      use skip_comment_f
!
      type(ctl_data_4_cub_kemo), intent(in) :: cubmesh_c
      type(size_of_cube), intent(inout) :: c_size
      type(ctl_param_plane_mesh), intent(inout) :: cube_p
!
      real(kind = kreal) :: pi
!
!
      pi = four * atan(one)
!
      if (cubmesh_c%cubmesh_plt%mesh_file_prefix%iflag .gt. 0) then
        cube_p%mesh_file_prefix                                         &
     &     = cubmesh_c%cubmesh_plt%mesh_file_prefix%charavalue
      else
        cube_p%mesh_file_prefix = 'mesh/in'
      end if
!
      if (cubmesh_c%ffile_cub_ctl%filter_head_ctl%iflag .eq. 1) then
        cube_p%filter_file_prefix                                       &
     &           = cubmesh_c%ffile_cub_ctl%filter_head_ctl%charavalue
      else
        cube_p%filter_file_prefix = 'mesh/filter_node_l'
      end if
!
      if(cubmesh_c%z_filter_head_ctl%iflag .eq. 1) then
        cube_p%z_filter_prefix = cubmesh_c%z_filter_head_ctl%charavalue
      else
        cube_p%z_filter_prefix = 'filter_info'
      end if
!
      call set_ctl_plane_parameter(cubmesh_c%cube_c, cube_p, c_size)
!
!
      if ( ( mod(c_size%nx_all,c_size%ndx) .ne. 0 ) .or.                &
     &     ( mod(c_size%ny_all,c_size%ndy) .ne. 0 ) .or.                &
     &     ( mod(c_size%nz_all,c_size%ndz) .ne. 0 )) then
        stop ' ***** error : illegal input '
      endif
      if (c_size%ndepth .ge. (c_size%nz_all / c_size%ndz) ) then
        write(*,*) 'neighbouring should be less than',                  &
    &        (c_size%nz_all / c_size%ndz)
        stop ' ***** error : illegal input '
      end if
!
      if (cubmesh_c%num_z_filter_ctl%iflag .gt. 0) then
        cube_p%iflag_filter = cubmesh_c%num_z_filter_ctl%intvalue
      else
        cube_p%iflag_filter = 0
      end if
!
      cube_p%iflag_z_filter = 0

!
      if (cubmesh_c%omitting_value_ctl%iflag .gt. 0) then
        cube_p%eps_filter = cubmesh_c%omitting_value_ctl%realvalue
      else
        cube_p%eps_filter = 0.0d0
      end if
!
!
      write(*,*) 'mesh_file_prefix:   ', trim(cube_p%mesh_file_prefix)
      write(*,*) 'filter_file_prefix: ',                                &
     &          trim(cube_p%filter_file_prefix)
      write(*,*) 'z_filter_prefix:    ', trim(cube_p%z_filter_prefix)
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
      write(*,*) 'omitting parameter: ', cube_p%eps_filter
!
      end subroutine s_set_ctl_data_plane_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_plane_parameter(cube_c, cube_p, c_size)
!
      use t_size_of_cube
      use t_ctl_data_4_plane_model
      use m_spheric_constants
      use skip_comment_f
!
      type(ctl_data_4_plane_model), intent(in) :: cube_c
      type(size_of_cube), intent(inout) :: c_size
      type(ctl_param_plane_mesh), intent(inout) :: cube_p
!
      real(kind = kreal) :: pi
      character(len = kchara) :: tmpchara
!
!
      pi = four * atan(one)
!
      c_size%xsize = 1.0d0
      c_size%ysize = 1.0d0
      c_size%zsize = 1.0d0
      if (cube_c%plane_size_ctl%iflag .gt. 0) then
        c_size%xsize = cube_c%plane_size_ctl%realvalue(1)
        c_size%ysize = cube_c%plane_size_ctl%realvalue(2)
        c_size%zsize = cube_c%plane_size_ctl%realvalue(3)
      end if
!
      tmpchara = cube_c%unit_len_plane_ctl%charavalue(1)
      if (cmp_no_case(tmpchara,'pi')) c_size%xsize = pi * c_size%xsize
!
      tmpchara = cube_c%unit_len_plane_ctl%charavalue(2)
      if (cmp_no_case(tmpchara,'pi')) c_size%ysize = pi * c_size%ysize
!
      tmpchara = cube_c%unit_len_plane_ctl%charavalue(3)
      if (cmp_no_case(tmpchara,'pi')) c_size%zsize = pi * c_size%zsize
!
      call set_plane_size(c_size)
!
      if (cube_c%nnod_plane_ctl%iflag .gt. 0) then
        c_size%nx_all = cube_c%nnod_plane_ctl%intvalue(1)
        c_size%ny_all = cube_c%nnod_plane_ctl%intvalue(2)
        c_size%nz_all = cube_c%nnod_plane_ctl%intvalue(3)
      else
        c_size%nx_all = 2
        c_size%ny_all = 2
        c_size%nz_all = 2
      end if
!
      if (cube_c%ndomain_plane_ctl%iflag .gt. 0) then
        c_size%ndx = cube_c%ndomain_plane_ctl%intvalue(1)
        c_size%ndy = cube_c%ndomain_plane_ctl%intvalue(2)
        c_size%ndz = cube_c%ndomain_plane_ctl%intvalue(3)
      else
        c_size%ndx = 1
        c_size%ndy = 1
        c_size%ndz = 1
      end if
!
      if (cube_c%num_of_sleeve_ctl%iflag .gt. 0) then
        c_size%ndepth = cube_c%num_of_sleeve_ctl%intvalue
      else
        c_size%ndepth = 1
      end if
!
      if      (cmp_no_case(cube_c%horizontal_grid_ctl%charavalue,       &
     &                     label_Chebyshev)) then
        cube_p%iflag_ztype = igrid_Chebyshev
      else if (cmp_no_case(cube_c%horizontal_grid_ctl%charavalue,       &
     &                     label_half_Cbyv)) then
        cube_p%iflag_ztype = igrid_half_Chebyshev
      else
        cube_p%iflag_ztype = igrid_equidistance
      end if
!
      end subroutine set_ctl_plane_parameter
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_plane_mesh
