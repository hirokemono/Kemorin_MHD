!
!      module set_ctl_data_plane_mesh
!
      module set_ctl_data_plane_mesh
!
!        programmed by H.Matsui on Aug., 2007
!
      use m_precision
      use m_constants
!
      implicit  none
!
!      subroutine s_set_ctl_data_plane_mesh
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_ctl_data_plane_mesh
!
      use m_size_4_plane
      use m_size_of_cube
      use m_cube_position
      use m_grp_data_cub_kemo
      use m_filtering_nod_4_cubmesh
      use m_cube_files_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_plane_model
      use m_ctl_data_4_cub_kemo
      use m_ctl_data_filter_files
!
!
      if (i_mesh_header .eq. 1) then
        mesh_file_header = mesh_file_prefix
      else
        mesh_file_header = 'mesh/in'
      end if
!
      if (i_filter_head_ctl .eq. 1) then
        filter_file_header = filter_head_ctl
      else
        filter_file_header = 'mesh/filter_node_l'
      end if
!
      if (i_z_filter_header .eq. 1) then
        z_filter_header = z_filter_head_ctl
      else
        z_filter_header = 'filter_info'
      end if
!
!
!
      if (i_plane_size_ctl .eq. 0) then
        plane_size_ctl(1:3) = 1.0d0
      end if
!
      pi = 4.0d0 * atan(1.0d0)
      if (   unit_len_plane_ctl(1) .eq. 'pi'                            &
     &  .or. unit_len_plane_ctl(1) .eq. 'Pi'                            &
     &  .or. unit_len_plane_ctl(1) .eq. 'PI') then
        xsize = pi*plane_size_ctl(1)
      else
        xsize = plane_size_ctl(1)
      end if
!
      if (   unit_len_plane_ctl(2) .eq. 'pi'                            &
     &  .or. unit_len_plane_ctl(2) .eq. 'Pi'                            &
     &  .or. unit_len_plane_ctl(2) .eq. 'PI') then
        ysize = pi*plane_size_ctl(2)
      else
        ysize = plane_size_ctl(2)
      end if
!
      if (   unit_len_plane_ctl(3) .eq. 'pi'                            &
     &  .or. unit_len_plane_ctl(3) .eq. 'Pi'                            &
     &  .or. unit_len_plane_ctl(3) .eq. 'PI') then
        zsize = pi*plane_size_ctl(3)
      else
        zsize = plane_size_ctl(3)
      end if
!
      xmax = xsize / two
      ymax = ysize / two
      zmax = zsize / two
      xmin = -xmax
      ymin = -ymax
      zmin = -zmax
!
      if (        horizontal_grid_ctl .eq. 'chebyshev'                  &
     &       .or. horizontal_grid_ctl .eq. 'Chebyshev'                  &
     &       .or. horizontal_grid_ctl .eq. 'CHEBYSHEV') then
        iradi = 2
      else if (   horizontal_grid_ctl .eq. 'half_chebyshev'             &
     &       .or. horizontal_grid_ctl .eq. 'half_Chebyshev'             &
     &       .or. horizontal_grid_ctl .eq. 'Half_Chebyshev'             &
     &       .or. horizontal_grid_ctl .eq. 'HALF_CHEBYSHEV') then
        iradi = 1
      else
        iradi = 0
      end if
!
!
      if (i_nnod_plane_ctl .eq. 1) then
        nx_all = nnod_plane_ctl(1)
        ny_all = nnod_plane_ctl(2)
        nz_all = nnod_plane_ctl(3)
      else
        nx_all = 2
        ny_all = 2
        nz_all = 2
      end if
!
!
      if (i_ndomain_plane_ctl .eq. 1) then
        ndx = ndomain_plane_ctl(1)
        ndy = ndomain_plane_ctl(2)
        ndz = ndomain_plane_ctl(3)
      else
        ndx = 1
        ndy = 1
        ndz = 1
      end if
!
      if ( ( mod(nx_all,ndx) /= 0 ) .or.                                &
     &     ( mod(ny_all,ndy) /= 0 ) .or.                                &
     &     ( mod(nz_all,ndz) /= 0 )      ) then
        stop ' ***** error : illegal input '
      endif
      if (neib .ge. (nz_all/ndz) ) then
        write(*,*) 'neighbouring should be less than', (nz_all/ndz)
        stop ' ***** error : illegal input '
      end if
!
      if (i_num_of_sleeve_ctl .eq. 1) then
        ndepth = num_of_sleeve_ctl
      else
        ndepth = 1
      end if
!
!
      if (i_num_z_filter.eq.1) then
        iflag_filter = num_z_filter_ctl
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
      if (i_omitting_value .eq. 1) then
        eps_filter = omitting_value_ctl
      else
        eps_filter = 0.0d0
      end if
!
!
      write(*,*) 'mesh_file_header:   ', trim(mesh_file_header)
      write(*,*) 'filter_file_header: ', trim(filter_file_header)
      write(*,*) 'z_filter_header:    ', trim(z_filter_header)
!
      write(*,'(a,1p3E25.15e3)') 'size of domain: ', xsize,ysize,zsize
      write(*,*) 'grid type: ', iradi
!
      write(*,*) 'num. of grid: ',   nx_all, ny_all, nz_all
      write(*,*) 'num. of domain: ', ndx, ndy, ndz
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
