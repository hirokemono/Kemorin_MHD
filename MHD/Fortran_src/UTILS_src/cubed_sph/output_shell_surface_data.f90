!
!      module output_shell_surface_data
!
      module output_shell_surface_data
!
!        programmed by H.Matsui on Apr., 2006
!
      use m_precision
!
      use m_cubed_sph_grp_param
!
      implicit  none
!
      integer(kind = kint), parameter :: id_sf_linear_mesh = 9
      integer(kind = kint), parameter :: id_sf_quad_mesh =  10
      integer(kind = kint), parameter :: id_sf_coarsing =   39
      private :: id_sf_linear_mesh, id_sf_quad_mesh
      private :: id_sf_coarsing
!
!      subroutine output_surface_data
!      subroutine output_surface_data_quad
!
!      subroutine output_surface_data_full(max_coarse_level)
!
!      subroutine output_circle_data
!      subroutine output_circle_data_quad
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine output_surface_data
!
      use m_cubed_sph_surf_mesh
!
      integer(kind = kint) :: inod, iele
!
!
      open (id_sf_linear_mesh,file='surf_connectivity.dat')
!
       write(id_sf_linear_mesh,'(a)')                                   &
     &  '! num. of node and edge for cube'
       write(id_sf_linear_mesh,'(5i16)') c_sphere1%numnod_cube,         &
     &   (c_sphere1%numedge_cube - c_sphere1%numedge_sf)
       write(id_sf_linear_mesh,'(a)')                                   &
     &  '! num. of node and edge for sphere, layer'
       write(id_sf_linear_mesh,'(5i16)') c_sphere1%numnod_sf,           &
     &   c_sphere1%numedge_sf, (c_sphere1%nele_shell+1)
!
       write(id_sf_linear_mesh,'(a)') '! number of element'
       write(id_sf_linear_mesh,'(i16)') c_sphere1%numele_sf
       write(id_sf_linear_mesh,'(a)')                                   &
     &  '! num. of layer for inner and outer core boundary'
       write(id_sf_linear_mesh,'(5i16)')                                &
     &              (nr_icb+1), (nr_cmb+1)
!
       write(id_sf_linear_mesh,'(a)') '! node'
       do inod = 1, c_sphere1%numnod_sf
        write(id_sf_linear_mesh,'(i16,1p3E25.15e3)') inod,              &
     &                              xyz_surf(inod,1:3)
       end do
!
       write(id_sf_linear_mesh,'(a)') '! connectivity'
       do iele = 1, c_sphere1%numele_sf
        write(id_sf_linear_mesh,'(5i16)') iele, ie_sf20(iele,1:4)
       end do
!
      close (id_sf_linear_mesh)
!
      end subroutine output_surface_data
!
!   --------------------------------------------------------------------
!
      subroutine output_surface_data_quad
!
      use m_cubed_sph_surf_mesh
!
      integer(kind = kint) :: inod, iele
!
!
      open (id_sf_quad_mesh,file='surf_connectivity_20.dat')
!
!
       write(id_sf_quad_mesh,'(a)')                                     &
     &  '! num. of node and edge for cube'
       write(id_sf_quad_mesh,'(5i16)') c_sphere1%numnod_cube,           &
     &    (c_sphere1%numedge_cube - c_sphere1%numedge_sf)
       write(id_sf_quad_mesh,'(a)')                                     &
     &  '! num. of node and edge for sphere, layer'
       write(id_sf_quad_mesh,'(5i16)') c_sphere1%numnod_sf,             &
     &     c_sphere1%numedge_sf, (c_sphere1%nele_shell+1)
!
       write(id_sf_quad_mesh,'(a)') '! number of element'
       write(id_sf_quad_mesh,'(i16)') c_sphere1%numele_sf
       write(id_sf_quad_mesh,'(a)')                                     &
     &  '! num. of layer for inner and outer core boundary'
       write(id_sf_quad_mesh,'(5i16)')                                  &
     &              (nr_icb+1), (nr_cmb+1)
!
       write(id_sf_quad_mesh,'(a)') '! node'
       do inod = 1, c_sphere1%numnod_sf20
        write(id_sf_quad_mesh,'(i16,1p3E25.15e3)') inod,                &
     &                              xyz_surf(inod,1:3)
       end do
!
       write(id_sf_quad_mesh,'(a)') '! connectivity'
       do iele = 1, c_sphere1%numele_sf
         write(id_sf_quad_mesh,'(9i16)') iele, ie_sf20(iele,1:8)
       end do
!
      close (id_sf_quad_mesh)
!
!
       end subroutine output_surface_data_quad
!
!   --------------------------------------------------------------------
!
      subroutine output_surface_data_full(max_coarse_level)
!
      use m_cubed_sph_surf_mesh
!
      integer(kind = kint), intent(in) :: max_coarse_level
      integer(kind = kint) :: ic, ist, ied, inod, iele, iele0, i, num
!
!
       open (id_sf_coarsing,file='surf_connectivity_m.dat')
!
       write(id_sf_coarsing,'(a)') '! coarse information'
       write(id_sf_coarsing,'(a)')  max_coarse_level
!
       write(id_sf_coarsing,'(a)') '! node stack for each level'
       write(id_sf_coarsing,'(10i16)')                                  &
     &         (c_sphere1%inod_stack_sf(ic),ic=1, max_coarse_level)
!
       write(id_sf_coarsing,'(a)') '! node table'
       do ic = 1, max_coarse_level
         ist = c_sphere1%inod_stack_sf(ic-1) + 1
         ied = c_sphere1%inod_stack_sf(ic)
         do inod = ist, ied
           write(id_sf_coarsing,'(3i16)')                               &
     &        (inod-c_sphere1%inod_stack_sf(ic-1)),                     &
     &         c_sphere1%inod_2_next(inod), c_sphere1%inod_2_org(inod)
         end do
       end do
!
!
       write(id_sf_coarsing,'(a)') '! element stack for each level'
       write(id_sf_coarsing,'(8i16)')                                   &
     &    (c_sphere1%iele_stack_sf(ic),ic=1, max_coarse_level)
!
       write(id_sf_coarsing,'(a)') '! connectivity'
       do ic = 1, max_coarse_level
         ist = c_sphere1%iele_stack_sf(ic-1) + 1
         ied = c_sphere1%iele_stack_sf(ic)
         do iele0 = ist, ied
           iele = iele0 + c_sphere1%numele_sf
            write(id_sf_coarsing,'(6i16)')                              &
     &        (iele0-c_sphere1%iele_stack_sf(ic-1)),                    &
     &        ie_sf20(iele,1:4)
         end do
       end do
!
!
!
       write(id_sf_coarsing,'(a)') '! merged element information'
       write(id_sf_coarsing,'(a)') '! number of merged element'
       do ic = 1, max_coarse_level
         ist = c_sphere1%iele_stack_sf(ic-1) + 1
         ied = c_sphere1%iele_stack_sf(ic)
         write(id_sf_coarsing,'(8i16)')                                 &
     &            (c_sphere1%num_merge_e_sf(iele),iele=ist,ied)
       end do
!
       write(id_sf_coarsing,'(a)') '! merged element id'
       do ic = 1, max_coarse_level
         ist = c_sphere1%iele_stack_sf(ic-1) + 1
         ied = c_sphere1%iele_stack_sf(ic)
         do iele = ist, ied
           iele0 = iele - c_sphere1%iele_stack_sf(ic-1)
           num = c_sphere1%num_merge_e_sf(iele)
           write(id_sf_coarsing,'(8i16)') iele0,                        &
     &         c_sphere1%imerge_e_sf(iele,1:num)
         end do
       end do
!
      close (id_sf_coarsing)
!
      end subroutine output_surface_data_full
!
!   --------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine output_circle_data
!
      use m_cubed_sph_surf_mesh
!
      integer(kind = kint) :: i, inod, iele
!
!
      open (id_sf_linear_mesh,file='edge_connectivity.dat')
!
       write(id_sf_linear_mesh,'(a)')                                   &
     &  '! num. of node and edge for cube'
       write(id_sf_linear_mesh,'(5i16)') c_sphere1%numnod_cube,         &
     &     (c_sphere1%numedge_cube - c_sphere1%numedge_sf)
       write(id_sf_linear_mesh,'(a)')                                   &
     &  '! num. of node and edge for sphere, layer'
       write(id_sf_linear_mesh,'(5i16)') c_sphere1%numnod_sf,           &
     &   c_sphere1%numedge_sf, (c_sphere1%nele_shell+1)
!
       write(id_sf_linear_mesh,'(a)') '! number of element'
       write(id_sf_linear_mesh,'(i16)') c_sphere1%ntot_edge_sf20
       write(id_sf_linear_mesh,'(a)')                                   &
     &  '! num. of layer for inner and outer core boundary'
       write(id_sf_linear_mesh,'(5i16)')                                &
     &              (nr_icb+1), (nr_cmb+1)
!
       write(id_sf_linear_mesh,'(a)') '! node'
       do inod = 1, c_sphere1%numnod_sf
        write(id_sf_linear_mesh,'(i16,1p3E25.15e3)') inod,              &
     &                          xyz_surf(inod,1:3)
       end do
!
       write(id_sf_linear_mesh,'(a)') '! connectivity'
       do iele = 1, c_sphere1%ntot_edge_sf20
        write(id_sf_linear_mesh,'(5i16)') iele, iedge_sf20(iele,1:3:2)
       end do
!
      close (id_sf_linear_mesh)
!
      end subroutine output_circle_data
!
!   --------------------------------------------------------------------
!
      subroutine output_circle_data_quad
!
      use m_cubed_sph_surf_mesh
!
      integer(kind = kint) :: inod, iele
!
!
      open (id_sf_quad_mesh,file='edge_connectivity_20.dat')
!
!
       write(id_sf_quad_mesh,'(a)')                                     &
     &  '! num. of node and edge for cube'
       write(id_sf_quad_mesh,'(5i16)') c_sphere1%numnod_cube,           &
     &     (c_sphere1%numedge_cube - c_sphere1%numedge_sf)
       write(id_sf_quad_mesh,'(a)')                                     &
     &  '! num. of node and edge for sphere, layer'
       write(id_sf_quad_mesh,'(5i16)') c_sphere1%numnod_sf,             &
     &     c_sphere1%numedge_sf, (c_sphere1%nele_shell+1)
!
       write(id_sf_quad_mesh,'(a)') '! number of element'
       write(id_sf_quad_mesh,'(i16)') c_sphere1%ntot_edge_sf20
       write(id_sf_quad_mesh,'(a)')                                     &
     &  '! num. of layer for inner and outer core boundary'
       write(id_sf_quad_mesh,'(5i16)')                                  &
     &              (nr_icb+1), (nr_cmb+1)
!
       write(id_sf_quad_mesh,'(a)') '! node'
       do inod = 1, c_sphere1%numnod_sf20
        write(id_sf_quad_mesh,'(i16,1p3E25.15e3)') inod,                &
     &                              xyz_surf(inod,1:3)
       end do
!
       write(id_sf_quad_mesh,'(a)') '! connectivity'
       do iele = 1, c_sphere1%ntot_edge_sf20
         write(id_sf_quad_mesh,'(9i16)') iele, iedge_sf20(iele,1:3)
       end do
!
      close (id_sf_quad_mesh)
!
       end subroutine output_circle_data_quad
!
!   --------------------------------------------------------------------
!
      end module output_shell_surface_data
