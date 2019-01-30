!const_cube_sphere_surface.f90
!
!      module const_cube_sphere_surface
!
!      Written by Kemorin on Apr., 2006
!
!      subroutine const_cube_surface_data
!      subroutine const_coarse_cube_surf_data
!
!
      module const_cube_sphere_surface
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
      subroutine const_cube_surface_data
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
      integer(kind= kint) :: inod_sf_end
      integer(kind= kint) :: iele_sf_end, irod_sf_end
!
!
       write(*,*) 'set_cube_skin'
      call set_cube_skin(inod_sf_end, c_sphere1)
!
      if (inod_sf_end .ne. c_sphere1%numnod_sf) then
       write(*,*) 'check the number of node in a sphere'
       stop
      end if
!
      call set_cube_rods(inod_sf_end, irod_sf_end, c_sphere1)
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
!   set connectivity for sphere surface
!
       write(*,*) 'connectivity construction for surface start'
       call set_cube_surf_connect(iele_sf_end, c_sphere1)
!
      if (iele_sf_end .ne. c_sphere1%numele_sf) then
        write(*,*) 'check the number of element in a sphere'
        stop
      end if
!
      write(*,*) 'output_surface_data'
      call output_surface_data(c_sphere1)
!
      if(iflag_quad .gt. 0) then
        write(*,*) 'output_surface_data_quad'
        call output_surface_data_quad(c_sphere1)
      end if
!
      end subroutine const_cube_surface_data
!
!   --------------------------------------------------------------------
!
      subroutine const_coarse_cube_surf_data
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
      integer(kind= kint) :: inod_sf_end, num
      integer(kind= kint) :: iele_sf_end, irod_sf_end
      integer(kind= kint) :: icoarse
!
!  construct coarse mesh information
!
      inod_sf_end = 0
      irod_sf_end = c_sphere1%numedge_sf
      iele_sf_end = c_sphere1%numele_sf
      do icoarse = 1, max_coarse_level
!
        call cal_coarse_cube_params(icoarse, c_sphere1)
!
        call set_coarse_cube_skin(inod_sf_end, c_sphere1)
        if (inod_sf_end .ne. c_sphere1%inod_stack_sf(icoarse) ) then
         write(*,*) 'check the number of node in a sphere... level:',   &
     &        icoarse, inod_sf_end, c_sphere1%inod_stack_sf(icoarse)
        stop
       end if
       write(*,*) 'inod_sf_end!!!', inod_sf_end
!
       call set_coarse_cube_surf_connect(iele_sf_end, c_sphere1)
       num = c_sphere1%iele_stack_sf(icoarse) + c_sphere1%numele_sf
       if(iele_sf_end .ne. num) then
        write(*,*) 'check the number of element in a sphere... level:'  &
     &           ,  icoarse
        stop
       end if
!
       write(*,*) 'set_merged_element_cube_surf'
       call set_merged_element_cube_surf(icoarse, c_sphere1)
!
      end do
!
      if (max_coarse_level .gt. 0) then
        write(*,*) 'output_surface_data_full'
        call output_surface_data_full(max_coarse_level, c_sphere1)
      end if
!
      end subroutine const_coarse_cube_surf_data
!
!   --------------------------------------------------------------------
!
      end module const_cube_sphere_surface
