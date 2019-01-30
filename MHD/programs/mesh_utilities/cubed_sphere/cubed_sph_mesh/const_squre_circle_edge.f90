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
       write(*,*) 'set_circle_node', c_sphere1%numnod_sf
      call set_circle_node(inod_sf_end, c_sphere1)
!
      if (inod_sf_end .ne. c_sphere1%numnod_sf) then
        write(*,*) 'check the number of node in a sphere'
        stop
      end if
!
      do inod = 1, inod_sf_end
        write(*,*) inod, c_sphere1%x_csph(inod,1:3)
      end do
!
      call set_circle_rods(inod_sf_end, irod_sf_end, c_sphere1)
!
      if (irod_sf_end .ne. c_sphere1%numedge_sf) then
        write(*,*) 'check the number of edge in a sphere'
        stop
      end if
      if (inod_sf_end .ne. c_sphere1%numnod_sf20) then
        write(*,*) 'check the number of node for quad in a sphere'
        stop
      end if
!
      call position_2_sph(c_sphere1%numnod_sf20, c_sphere1%x_csph,      &
     &    c_sphere1%r_csph, c_sphere1%theta_csph, c_sphere1%phi_csph,   &
     &    c_sphere1%ar_csph, c_sphere1%s_csph, c_sphere1%as_csph)
!
      write(*,*) 'output_circle_data'
      call output_circle_data(c_sphere1)
!
      if(iflag_quad .gt. 0) then
        write(*,*) 'output_circle_data_quad'
        call output_circle_data_quad(c_sphere1)
      end if
!
      end subroutine const_squre_circle_edge_data
!
!   --------------------------------------------------------------------
!
      end module const_squre_circle_edge
