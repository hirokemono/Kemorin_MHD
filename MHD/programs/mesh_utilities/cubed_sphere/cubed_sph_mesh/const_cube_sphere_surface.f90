!const_cube_sphere_surface.f90
!
!      module const_cube_sphere_surface
!
!      Written by Kemorin on Apr., 2006
!
!!      subroutine const_cube_surface_data(csph_p, c_sphere)
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!      subroutine const_coarse_cube_surf_data                          &
!!     &         (csph_mesh, csph_p, course_p, c_sphere)
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!        type(numref_cubed_sph), intent(in) :: csph_p
!!        type(coarse_cubed_sph), intent(inout) :: course_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      module const_cube_sphere_surface
!
      use m_precision
!
      use t_cubed_sph_mesh
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
      subroutine const_cube_surface_data(csph_p, c_sphere)
!
      use t_numref_cubed_sph
!
      use output_shell_surface_data
      use set_cube_surface
      use coordinate_converter
!
      use set_surface_rods_sphere
      use set_surf_connect_cubed_sph
!
      type(numref_cubed_sph), intent(inout) :: csph_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind= kint) :: inod_sf_end
      integer(kind= kint) :: iele_sf_end, irod_sf_end
!
!
       write(*,*) 'set_cube_skin'
      call set_cube_skin(inod_sf_end, csph_p, c_sphere)
!
      if (inod_sf_end .ne. c_sphere%numnod_sf) then
       write(*,*) 'check the number of node in a sphere'
       stop
      end if
!
      call set_cube_rods(csph_p%num_hemi, csph_p%x_edge,                &
     &    inod_sf_end, irod_sf_end, c_sphere)
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
!   set connectivity for sphere surface
!
       write(*,*) 'connectivity construction for surface start'
       call set_cube_surf_connect                                       &
     &    (csph_p%num_hemi, iele_sf_end, c_sphere)
!
      if (iele_sf_end .ne. c_sphere%numele_sf) then
        write(*,*) 'check the number of element in a sphere'
        stop
      end if
!
      write(*,*) 'output_surface_data'
      call output_surface_data(c_sphere)
!
      if(csph_p%iflag_quad .gt. 0) then
        write(*,*) 'output_surface_data_quad'
        call output_surface_data_quad(c_sphere)
      end if
!
      end subroutine const_cube_surface_data
!
!   --------------------------------------------------------------------
!
      subroutine const_coarse_cube_surf_data                            &
     &         (csph_mesh, csph_p, course_p, c_sphere)
!
      use t_numref_cubed_sph
!
      use output_shell_surface_data
      use count_coarse_parameters
      use set_coarsed_cube_skin
      use merged_ele_4_cubed_sph_surf
      use set_surf_connect_cubed_sph
!
      type(cubed_sph_mesh), intent(in) :: csph_mesh
      type(numref_cubed_sph), intent(in) :: csph_p
      type(coarse_cubed_sph), intent(inout) :: course_p
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
      do icoarse = 1, course_p%max_coarse_level
!
        call cal_coarse_cube_params                                     &
     &     (icoarse, c_sphere, csph_mesh, csph_p, course_p)
!
        call set_coarse_cube_skin                                       &
     &     (csph_p%num_hemi, course_p%nskip_s, course_p%nskip_fs,       &
     &      inod_sf_end, c_sphere)
        if (inod_sf_end .ne. c_sphere%inod_stack_sf(icoarse) ) then
         write(*,*) 'check the number of node in a sphere... level:',   &
     &        icoarse, inod_sf_end, c_sphere%inod_stack_sf(icoarse)
        stop
       end if
       write(*,*) 'inod_sf_end!!!', inod_sf_end
!
       call set_coarse_cube_surf_connect                                &
     &    (course_p%n_hemi_c, iele_sf_end, c_sphere)
       num = c_sphere%iele_stack_sf(icoarse) + c_sphere%numele_sf
       if(iele_sf_end .ne. num) then
        write(*,*) 'check the number of element in a sphere... level:'  &
     &           ,  icoarse
        stop
       end if
!
       write(*,*) 'set_merged_element_cube_surf'
       call set_merged_element_cube_surf                                &
     &    (icoarse, csph_p, course_p, c_sphere)
!
      end do
!
      if (course_p%max_coarse_level .gt. 0) then
        write(*,*) 'output_surface_data_full'
        call output_surface_data_full                                   &
     &     (course_p%max_coarse_level, c_sphere)
      end if
!
      end subroutine const_coarse_cube_surf_data
!
!   --------------------------------------------------------------------
!
      end module const_cube_sphere_surface
