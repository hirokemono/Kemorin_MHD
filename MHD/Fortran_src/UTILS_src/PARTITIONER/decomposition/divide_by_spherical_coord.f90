!
!      module divide_by_spherical_coord
!
!      Written by H. Matsui
!
!!      subroutine divide_by_sphere_coord(part_p, nnod, nnod_4_ele, xx, &
!!     &          radius, theta, phi, num_bc_grp, ntot_bc_grp,          &
!!     &          istack_bc_grp, item_bc_grp, name_bc_grp, nod_d_grp)
!!        type(ctl_param_partitioner), intent(in) :: part_p
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!        output: nod_d_grp%IGROUP
!
      module divide_by_spherical_coord
!
      use m_precision
!
      implicit none
!
      real(kind=kreal), allocatable, private :: VAL_sph(:)
      integer(kind=kint), allocatable, private :: IS_sph(:)
!
      real(kind=kreal), allocatable, private :: VAL_cube(:)
      integer(kind=kint), allocatable, private :: IS_cube(:)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine divide_by_sphere_coord(part_p, nnod, nnod_4_ele, xx,   &
     &          radius, theta, phi, num_bc_grp, ntot_bc_grp,            &
     &          istack_bc_grp, item_bc_grp, name_bc_grp, nod_d_grp)
!
      use m_geometry_constants
      use t_ctl_param_partitioner
      use t_shell_surface_4_part
      use t_domain_group_4_partition
!
      use sort_sphere_4_rcb
      use sort_cube_4_rcb
      use cal_minmax_and_stacks
      use find_shell_information
      use grouping_by_sphere
      use set_sphere_data
!
      type(ctl_param_partitioner), intent(in) :: part_p
      integer(kind = kint), intent(in)  :: nnod, nnod_4_ele
      real(kind= kreal), intent(in) :: xx(nnod,3)
      real(kind= kreal), intent(in) :: radius(nnod)
      real(kind= kreal), intent(in) :: theta(nnod)
      real(kind= kreal), intent(in) :: phi(nnod)
!
      integer(kind = kint), intent(in) :: num_bc_grp, ntot_bc_grp
      integer(kind = kint), intent(in) :: istack_bc_grp(0:num_bc_grp)
      integer(kind = kint), intent(in) :: item_bc_grp(ntot_bc_grp)
      character(len=kchara), intent(in) :: name_bc_grp(num_bc_grp)
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      type(shell_surface_4_part) :: sphere_4_part
      integer(kind = kint)  :: irest1, num1
!
!C
!C +------------------------------+
!C | RCB for spherical coordinate |
!C +------------------------------+
!C===
!
      call alloc_local_nnod_shell(part_p%num_domain, sphere_4_part)
!
      call cal_divide_and_rest(num1, irest1, nnod, part_p%num_domain)
      call set_number_of_segments                                       &
     &   (part_p%num_domain, num1, irest1, sphere_4_part%numnod_local)
!
!
      if (nnod_4_ele.ne.num_t_linear                                    &
     &     .and. part_p%iflag_sphere_data.eq.0) then
        write(*,*) 'Sphere correction data is required!'
        stop
      end if
      if(part_p%ndivide_eb(1).gt.1                                      &
     &     .and. part_p%iflag_sphere_data.eq.0) then
        write(*,*) 'Use sphere correction data.'
        stop
      end if
!
      if (part_p%iflag_sphere_data.eq. 0) then
        call s_find_shell_information                                   &
     &     (part_p%ndivide_eb(2), part_p%ndivide_eb(3),                 &
     &      nnod, radius, theta, phi, num_bc_grp, ntot_bc_grp,          &
     &      istack_bc_grp, item_bc_grp, name_bc_grp, sphere_4_part)
      else
        if (nnod_4_ele .eq. 8) then
          call set_sphere_data_4_linear(part_p%sphere_data_file_name,   &
     &        part_p%ndivide_eb(2), part_p%ndivide_eb(3),               &
     &        sphere_4_part)
        else if (nnod_4_ele .eq. 20) then
          call set_sphere_data_4_quad(part_p%sphere_data_file_name,     &
     &        part_p%ndivide_eb(2), part_p%ndivide_eb(3),               &
     &        sphere_4_part)
        end if
      end if
!
      allocate (VAL_sph(sphere_4_part%num_CMB))
      allocate (IS_sph(sphere_4_part%num_CMB))

      allocate (VAL_cube(nnod))
      allocate (IS_cube(nnod))

      nod_d_grp%IGROUP(1:nnod)= 1

!   grouping radial layer
!
        call part_sphere_with_radius(part_p%ndivide_eb(1),              &
     &      sphere_4_part%IGROUP_radius, sphere_4_part%num_layer,       &
     &      sphere_4_part%nlayer_ICB, sphere_4_part%nlayer_CMB)
!
!   grouping nodes on sphere
!
        write(*,*) 's_sort_sphere_4_rcb'
        call s_sort_sphere_4_rcb                                        &
     &     (sphere_4_part%num_CMB, sphere_4_part%IGROUP_cmb,            &
     &      part_p%ndivide_eb(2), part_p%ndivide_eb(3),                 &
     &      sphere_4_part%rtp_cmb(1,2), sphere_4_part%rtp_cmb(1,3),     &
     &      VAL_sph, IS_sph)
!
!       write(*,*) 'IGROUP_cmb', sphere_4_part%IGROUP_cmb
!   grouping nodes in shell
!
        write(*,*) 'set_sphere_domain_list_l'
        call set_sphere_domain_list_l                                   &
     &     (part_p%num_domain, nnod, part_p%ndivide_eb(1),              &
     &      sphere_4_part%num_CMB, sphere_4_part%nnod_CMB,              &
     &      sphere_4_part%num_layer, sphere_4_part%istack_sph,          &
     &      sphere_4_part%item_sph, sphere_4_part%IGROUP_cmb,           &
     &      sphere_4_part%IGROUP_radius, nod_d_grp%IGROUP,              &
     &      sphere_4_part%ncore_local)
!
        if (nnod_4_ele .eq. 20) then
          write(*,*) 'set_sphere_domain_list_q'
          call set_sphere_domain_list_q                                 &
     &       (part_p%num_domain, nnod, part_p%ndivide_eb(1),            &
     &        sphere_4_part%num_CMB, sphere_4_part%num_layer,           &
     &        sphere_4_part%istack20_sph, sphere_4_part%item20_sph,     &
     &        sphere_4_part%IGROUP_cmb, sphere_4_part%IGROUP_radius,    &
     &        nod_d_grp%IGROUP, sphere_4_part%ncore_local)
        end if
!
!   count nuber of node for each subdomain
!
        write(*,*) 'count_num_subdomain_4_cube'
        call count_num_subdomain_4_cube                                 &
     &     (part_p%num_domain, sphere_4_part%numnod_local,              &
     &      sphere_4_part%ncore_local, sphere_4_part%nrest_local)
!
!  devide around center
!
        call s_sort_cube_4_rcb                                          &
     &     (nnod, sphere_4_part%num_cube, part_p%num_domain,            &
     &      part_p%ndivide_eb(1), part_p%ndivide_eb(2),                 &
     &      part_p%ndivide_eb(3), xx, phi,                              &
     &      sphere_4_part%inod_free, sphere_4_part%nrest_local,         &
     &      nod_d_grp%IGROUP, IS_cube, VAL_cube)
!
      deallocate (VAL_sph, IS_sph)
      deallocate (VAL_cube, IS_cube)
!
      if (nnod_4_ele .eq. 20) call dealloc_quad_shell(sphere_4_part)
      call dealloc_linear_shell(sphere_4_part)
      call dealloc_local_nnod_shell(sphere_4_part)
!
      end subroutine divide_by_sphere_coord
!
!   --------------------------------------------------------------------
!
      end module divide_by_spherical_coord
