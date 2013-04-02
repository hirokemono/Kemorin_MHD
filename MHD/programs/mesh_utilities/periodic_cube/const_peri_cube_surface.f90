!const_peri_cube_surface.f90
!      module const_peri_cube_surface
!      Written by Kemorin on Apr., 2006
!
!      subroutine const_peri_cube_surface
!      subroutine const_coarse_rect_surf_data
!
!
      module const_peri_cube_surface
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
      subroutine const_rect_sphere_surf_data
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_cube_surface
      use set_surface_rods_sphere
      use set_surf_connect_cubed_sph
      use coordinate_converter
      use output_shell_surface_data
!
      integer(kind= kint) :: inod_sf_end, irod_sf_end, iele_sf_end
      integer(kind= kint) :: i
!
!
       write(*,*) 'set_rect_skin'
      call set_rect_skin(inod_sf_end)
!
      if (inod_sf_end.ne.numnod_sf) then
       write(*,*) 'check the number of node in a sphere'
       stop
      end if
!
      call set_rect_rods(inod_sf_end, irod_sf_end)
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
!   set connectivity for sphere surface
!
       write(*,*) 'connectivity construction for surface'
       call set_rect_surf_connect(iele_sf_end)
!
      if (iele_sf_end.ne.numele_sf) then
        write(*,*) 'check the number of element in a sphere'
        stop
      end if
!
      write(*,*) 'output_surface_data'
      call output_surface_data
!
      if(iflag_quad .gt. 0) then
        write(*,*) 'output_surface_data_quad'
        call output_surface_data_quad
      end if
!
      end subroutine const_rect_sphere_surf_data
!
!   --------------------------------------------------------------------
!
      subroutine const_coarse_rect_surf_data
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
!
      use output_shell_surface_data
      use count_coarse_parameters
      use set_coarsed_cube_skin
      use merged_ele_4_cubed_sph_surf
      use set_surf_connect_cubed_sph
!
      integer(kind= kint) :: inod_sf_end
      integer(kind= kint) :: iele_sf_end, irod_sf_end
      integer(kind= kint) :: icoarse
!
!  construct coarse mesh information
!
      inod_sf_end = 0
      irod_sf_end = numedge_sf
      iele_sf_end = numele_sf
      do icoarse = 1, max_coarse_level
!
        call cal_coarse_rect_params(icoarse)
!
        call set_coarse_rect_skin(inod_sf_end)
        if (inod_sf_end .ne. inod_stack_sf(icoarse) ) then
          write(*,*) 'check the number of node in a sphere... level:'   &
     &           ,  icoarse, inod_sf_end, inod_stack_sf(icoarse)
          stop
        end if
        write(*,*) 'inod_sf_end!!!', inod_sf_end
!
        call set_coarse_rect_surf_connect(iele_sf_end)
        if (iele_sf_end .ne.(iele_stack_sf(icoarse)+numele_sf) ) then
         write(*,*) 'check the number of element in a sphere... level:' &
     &           ,  icoarse
          stop
        end if
!
       write(*,*) 'set_merged_element_rect_surf'
       call set_merged_element_rect_surf(icoarse)
!
      end do
!
      if (max_coarse_level .gt. 0) then
        write(*,*) 'output_surface_data_full'
        call output_surface_data_full(max_coarse_level)
      end if
!
      end subroutine const_coarse_rect_surf_data
!
!   --------------------------------------------------------------------
!
      end module const_peri_cube_surface
