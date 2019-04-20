!write_cubed_sph_grp_data.f90
!      module write_cubed_sph_grp_data
!
!      Written by Kemorin on Apr., 2006
!
!!      subroutine output_group_data(num_hemi, c_sphere, csph_grp)
!!      subroutine output_group_data_quad                               &
!!     &         (num_hemi, c_sphere, csph_mesh, csph_grp)
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!        type(cubed_sph_group), intent(in) :: csph_grp
!!      subroutine output_coarse_group_data(course_p, csph_grp)
!!        type(coarse_cubed_sph), intent(in) :: course_p
!
      module write_cubed_sph_grp_data
!
      use m_precision
!
      use m_constants
!
      use t_numref_cubed_sph
      use t_mesh_data
      use t_group_data
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_mesh
      use t_cubed_sph_grp_param
!
      use cubed_sph_file_names
      use set_node_groups_4_shell
      use set_element_groups_4_shell
      use set_surface_groups_4_shell
!
      implicit none
!
      type(mesh_groups), save, private :: group_csph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine output_group_data(num_hemi, c_sphere, csph_grp)
!
      integer(kind = kint), intent(in) :: num_hemi
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_group), intent(in) :: csph_grp
!
!   set groups
!
      write(*,*) 'output group information for linear'
!
      call count_cubed_sph_node_grp_l                                   &
     &   (c_sphere%numnod_cube, c_sphere%numnod_sf,                     &
     &    ione, csph_grp%csp_nod_grp, group_csph%nod_grp)
!
      call alloc_group_num(group_csph%nod_grp)
      call set_cubed_sph_node_grp_names                                 &
     &   (csph_grp%csp_nod_grp, group_csph%nod_grp)
      call set_cubed_sph_node_istack_l                                  &
     &   (c_sphere%numnod_cube, c_sphere%numnod_sf,                     &
     &    ione, csph_grp%csp_nod_grp, group_csph%nod_grp)
!
      call alloc_group_item(group_csph%nod_grp)
      call set_cubed_sph_nodal_item_l                                   &
     &   (c_sphere%numnod_cube, c_sphere%numnod_sf, num_hemi, ione,     &
     &    csph_grp%nr_cmb, csph_grp%csp_nod_grp, group_csph%nod_grp)
!
      call write_cubed_sph_nod_grp(id_l_group, group_csph%nod_grp)
!
!   set element group
!
      call count_cubed_sph_ele_grps                                     &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, csph_grp%csp_ele_grp, group_csph%ele_grp)
!
      call alloc_group_num(group_csph%ele_grp)
      call set_cubed_sph_ele_grp_names                                  &
     &   (csph_grp%csp_ele_grp, group_csph%ele_grp)
      call set_cubed_sph_ele_grp_istack                                 &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, csph_grp%csp_ele_grp, group_csph%ele_grp)
!
      call alloc_group_item(group_csph%ele_grp)
      call set_cubed_sph_ele_grp_item                                   &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, csph_grp%csp_ele_grp, group_csph%ele_grp)
!
      call write_element_group(id_l_group, group_csph%ele_grp)
!
!
! surface group
!
      call count_cubed_sph_surf_groups(c_sphere%numele_sf, ione,        &
     &   csph_grp%csp_surf_grp, group_csph%surf_grp)
!
      call alloc_sf_group_num(group_csph%surf_grp)
      call set_cubed_sph_surf_grp_names                                 &
     &   (csph_grp%csp_surf_grp, group_csph%surf_grp)
      call set_cubed_sph_surf_istack(c_sphere%numele_sf, ione,          &
     &    csph_grp%csp_surf_grp, group_csph%surf_grp)
!
      call alloc_sf_group_item(group_csph%surf_grp)
      call set_cubed_sph_surf_item                                      &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, csph_grp%csp_surf_grp, group_csph%surf_grp)
!
      call write_surf_grp_shell(id_l_group, group_csph%surf_grp)
!
      call dealloc_groups_data(group_csph)
!
      return
      end subroutine output_group_data
!
!   --------------------------------------------------------------------
!
      subroutine output_group_data_quad                                 &
     &         (num_hemi, c_sphere, csph_mesh, csph_grp)
!
      integer(kind = kint), intent(in) :: num_hemi
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_mesh), intent(in) :: csph_mesh
      type(cubed_sph_group), intent(in) :: csph_grp
!
!   set groups
!
      write(*,*) 'output quad group information'
!
      call count_cubed_sph_node_grp_q                                   &
     &   (c_sphere%numnod_cube, c_sphere%numedge_cube,                  &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf,                      &
     &    csph_grp%csp_nod_grp, group_csph%nod_grp)
!
      call alloc_group_num(group_csph%nod_grp)
      call set_cubed_sph_node_grp_names                                 &
     &   (csph_grp%csp_nod_grp, group_csph%nod_grp)
      call set_cubed_sph_node_istack_q                                  &
     &   (c_sphere%numnod_cube, c_sphere%numedge_cube,                  &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf,                      &
     &    csph_grp%csp_nod_grp, group_csph%nod_grp)
!
      call alloc_group_item(group_csph%nod_grp)
      call set_cubed_sph_nodal_item_q(csph_mesh%nnod_cb_sph,            &
     &    c_sphere%numnod_cube, c_sphere%numedge_cube,                  &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf, num_hemi, ione,      &
     &    csph_grp%nr_cmb, csph_grp%csp_nod_grp, group_csph%nod_grp)
!
      call write_cubed_sph_nod_grp(id_q_group, group_csph%nod_grp)
!
!   set element group
!
      call count_cubed_sph_ele_grps                                     &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, csph_grp%csp_ele_grp, group_csph%ele_grp)
!
      call alloc_group_num(group_csph%ele_grp)
      call set_cubed_sph_ele_grp_names                                  &
     &   (csph_grp%csp_ele_grp, group_csph%ele_grp)
      call set_cubed_sph_ele_grp_istack                                 &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, csph_grp%csp_ele_grp, group_csph%ele_grp)
!
      call alloc_group_item(group_csph%ele_grp)
      call set_cubed_sph_ele_grp_item                                   &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, csph_grp%csp_ele_grp, group_csph%ele_grp)
!
      call write_element_group(id_q_group, group_csph%ele_grp)
!
! surface group
!
      call count_cubed_sph_surf_groups(c_sphere%numele_sf, ione,        &
     &    csph_grp%csp_surf_grp, group_csph%surf_grp)
!
      call alloc_sf_group_num(group_csph%surf_grp)
      call set_cubed_sph_surf_grp_names                                 &
     &   (csph_grp%csp_surf_grp, group_csph%surf_grp)
      call set_cubed_sph_surf_istack(c_sphere%numele_sf, ione,          &
     &    csph_grp%csp_surf_grp, group_csph%surf_grp)
!
      call alloc_sf_group_item(group_csph%surf_grp)
      call set_cubed_sph_surf_item                                      &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, csph_grp%csp_surf_grp, group_csph%surf_grp)
!
      call write_surf_grp_shell(id_q_group, group_csph%surf_grp)
!
      call dealloc_groups_data(group_csph)
!
      end subroutine output_group_data_quad
!
!   --------------------------------------------------------------------
!
      subroutine output_coarse_group_data(course_p, csph_grp)
!
      type(coarse_cubed_sph), intent(in) :: course_p
      type(cubed_sph_group), intent(in) :: csph_grp
!
!   set groups
!
      call count_cubed_sph_node_grp_l                                   &
     &   (course_p%nnod_cube_c, course_p%nnod_sf_c, course_p%nskip_r,   &
     &    csph_grp%csp_nod_grp, group_csph%nod_grp)
!
      call alloc_group_num(group_csph%nod_grp)
      call set_cubed_sph_node_grp_names                                 &
     &   (csph_grp%csp_nod_grp, group_csph%nod_grp)
      call set_cubed_sph_node_istack_l                                  &
     &   (course_p%nnod_cube_c, course_p%nnod_sf_c, course_p%nskip_r,   &
     &    csph_grp%csp_nod_grp, group_csph%nod_grp)
!
      call alloc_group_item(group_csph%nod_grp)
      call set_cubed_sph_nodal_item_l                                   &
     &   (course_p%nnod_cube_c, course_p%nnod_sf_c, course_p%n_hemi_c,  &
     &    course_p%nskip_r, csph_grp%nr_cmb, csph_grp%csp_nod_grp,      &
     &    group_csph%nod_grp)
!
      call write_cubed_sph_nod_grp(id_l_group, group_csph%nod_grp)
!
!
!   set element group
!
      call count_cubed_sph_ele_grps                                     &
     &   (course_p%nele_cube_c, course_p%nele_sf_c, course_p%nskip_r,   &
     &    csph_grp%csp_ele_grp, group_csph%ele_grp)
!
      call alloc_group_num(group_csph%ele_grp)
      call set_cubed_sph_ele_grp_names                                  &
     &   (csph_grp%csp_ele_grp, group_csph%ele_grp)
      call set_cubed_sph_ele_grp_istack                                 &
     &   (course_p%nele_cube_c, course_p%nele_sf_c, course_p%nskip_r,   &
     &    csph_grp%csp_ele_grp, group_csph%ele_grp)
!
      call alloc_group_item(group_csph%ele_grp)
      call set_cubed_sph_ele_grp_item                                   &
     &   (course_p%nele_cube_c, course_p%nele_sf_c, course_p%nskip_r,   &
     &    csph_grp%csp_ele_grp, group_csph%ele_grp)
!
      call write_element_group(id_l_group, group_csph%ele_grp)
!
!
! surface group
!
      call count_cubed_sph_surf_groups(course_p%nele_sf_c,              &
     &    course_p%nskip_r, csph_grp%csp_surf_grp, group_csph%surf_grp)
!
      call alloc_sf_group_num(group_csph%surf_grp)
      call set_cubed_sph_surf_grp_names                                 &
     &   (csph_grp%csp_surf_grp, group_csph%surf_grp)
      call set_cubed_sph_surf_istack(course_p%nele_sf_c,                &
     &    course_p%nskip_r, csph_grp%csp_surf_grp, group_csph%surf_grp)
!
      call alloc_sf_group_item(group_csph%surf_grp)
      call set_cubed_sph_surf_item                                      &
     &   (course_p%nele_cube_c, course_p%nele_sf_c, course_p%nskip_r,   &
     &    csph_grp%csp_surf_grp, group_csph%surf_grp)
!
      call write_surf_grp_shell(id_l_group, group_csph%surf_grp)
!
      call dealloc_groups_data(group_csph)
!
      end subroutine output_coarse_group_data
!
!   --------------------------------------------------------------------
!
      end module write_cubed_sph_grp_data
