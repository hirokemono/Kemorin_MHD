!const_squre_circle_edge.f90
!
!      module const_squre_circle_edge
!
!      Written by Kemorin on Apr., 2006
!
!      subroutine const_squre_circle_edge_data
!
!
      module const_squre_circle_edge
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine const_squre_circle_edge_data
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
!
      use output_shell_surface_data
      use set_cube_surface
      use coordinate_converter
!
      use set_surface_rods_sphere
      use set_surf_connect_cubed_sph
!
      integer(kind= kint) :: inod_sf_end, irod_sf_end
      integer(kind= kint) :: inod
!
!
       write(*,*) 'set_circle_node', numnod_sf
      call set_circle_node(inod_sf_end)
!
      if (inod_sf_end.ne.numnod_sf) then
        write(*,*) 'check the number of node in a sphere'
        stop
      end if
!
      do inod = 1, inod_sf_end
        write(*,*) inod, xyz_surf(inod,1:3)
      end do
!
      call set_circle_rods(inod_sf_end, irod_sf_end)
!
      if (irod_sf_end.ne.numedge_sf) then
        write(*,*) 'check the number of edge in a sphere'
        stop
      end if
      if (inod_sf_end.ne.numnod_sf20) then
        write(*,*) 'check the number of node for quad in a sphere'
        stop
      end if
!
      call position_2_sph (numnod_sf20, xyz_surf, r_surf, theta_surf,   &
     &     phi_surf, ar_surf, s_surf, as_surf)
!
      write(*,*) 'output_circle_data'
      call output_circle_data
!
      if(iflag_quad .gt. 0) then
        write(*,*) 'output_circle_data_quad'
        call output_circle_data_quad
      end if
!
      end subroutine const_squre_circle_edge_data
!
!   --------------------------------------------------------------------
!
      end module const_squre_circle_edge
