!const_rect_sphere_surface.f90
!      module const_rect_sphere_surface
!      Written by Kemorin on Apr., 2006
!
!!      subroutine const_rect_sphere_surf_node(rad_edge, c_sphere)
!!      subroutine const_rect_sphere_surf_data(c_sphere)
!!      subroutine const_coarse_rect_surf_data(c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      module const_rect_sphere_surface
!
      use m_precision
!
      use t_cubed_sph_surf_mesh
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine const_rect_sphere_surf_node(rad_edge, c_sphere)
!
      use m_numref_cubed_sph
      use set_cube_surface
      use set_surface_rods_sphere
      use set_surf_connect_cubed_sph
      use coordinate_converter
      use output_shell_surface_data
!
      real(kind = kreal), intent(in) :: rad_edge
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind= kint) :: inod_sf_end, irod_sf_end
!
!
      call set_rect_skin(rad_edge, inod_sf_end, c_sphere)
!
      if (inod_sf_end .ne. c_sphere%numnod_sf) then
       write(*,*) 'check the number of node in a sphere'
       stop
      end if
!
      call set_rect_rods(inod_sf_end, irod_sf_end, c_sphere)
!
      if (irod_sf_end .ne. c_sphere%numedge_sf) then
        write(*,*) 'check the number of edge in a sphere'
        stop
      end if
      if (inod_sf_end .ne. c_sphere%numnod_sf20) then
        write(*,*) 'check the number of node for quad in a sphere'
        stop
      end if
!
      call position_2_sph(c_sphere%numnod_sf20, c_sphere%x_csph,        &
     &    c_sphere%r_csph, c_sphere%theta_csph, c_sphere%phi_csph,      &
     &    c_sphere%ar_csph, c_sphere%s_csph, c_sphere%as_csph)
!
      end subroutine const_rect_sphere_surf_node
!
!   --------------------------------------------------------------------
!
      subroutine const_rect_sphere_surf_data(c_sphere)
!
      use m_numref_cubed_sph
      use set_cube_surface
      use set_surface_rods_sphere
      use set_surf_connect_cubed_sph
      use coordinate_converter
      use output_shell_surface_data
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind= kint) :: iele_sf_end
!
!   set connectivity for sphere surface
!
       write(*,*) 'connectivity construction for surface'
       call set_rect_surf_connect(iele_sf_end, c_sphere)
!
      if (iele_sf_end .ne. c_sphere%numele_sf) then
        write(*,*) 'check the number of element in a sphere'
        stop
      end if
!
      write(*,*) 'output_surface_data'
      call output_surface_data(c_sphere)
!
      if(iflag_quad .gt. 0) then
        write(*,*) 'output_surface_data_quad'
        call output_surface_data_quad(c_sphere)
      end if
!
      end subroutine const_rect_sphere_surf_data
!
!   --------------------------------------------------------------------
!
      subroutine const_coarse_rect_surf_data(c_sphere)
!
      use m_numref_cubed_sph
!
      use output_shell_surface_data
      use count_coarse_parameters
      use set_coarsed_cube_skin
      use merged_ele_4_cubed_sph_surf
      use set_surf_connect_cubed_sph
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind= kint) :: inod_sf_end, num
      integer(kind= kint) :: iele_sf_end, irod_sf_end
      integer(kind= kint) :: icoarse
!
!  construct coarse mesh information
!
      inod_sf_end = 0
      irod_sf_end = c_sphere%numedge_sf
      iele_sf_end = c_sphere%numele_sf
      do icoarse = 1, max_coarse_level
        call cal_coarse_rect_params(icoarse, c_sphere)
!
        call set_coarse_rect_skin(inod_sf_end, c_sphere)
        if(inod_sf_end .ne. c_sphere%inod_stack_sf(icoarse)) then
          write(*,*) 'check the number of node in a sphere... level:',  &
     &       icoarse, inod_sf_end, c_sphere%inod_stack_sf(icoarse)
          stop
        end if
        write(*,*) 'inod_sf_end!!!', inod_sf_end
!
        call set_coarse_rect_surf_connect(iele_sf_end, c_sphere)
        num = c_sphere%iele_stack_sf(icoarse) + c_sphere%numele_sf
        if (iele_sf_end .ne. num) then
         write(*,*)                                                     &
     &     'check the number of element in a sphere... level:',         &
     &     icoarse
          stop
        end if
!
       write(*,*) 'set_merged_element_rect_surf'
       call set_merged_element_rect_surf(icoarse, c_sphere)
!
      end do
!
      if (max_coarse_level .gt. 0) then
        write(*,*) 'output_surface_data_full'
        call output_surface_data_full(max_coarse_level, c_sphere)
      end if
!
      end subroutine const_coarse_rect_surf_data
!
!   --------------------------------------------------------------------
!
      end module const_rect_sphere_surface
