!
!      module volume_rendering
!
!      Written by H. Matsui on July, 2006
!
!      subroutine pvr_init(numnod, numele, numsurf,                     &
!     &          nnod_4_surf, inod_smp_stack, xx,                       &
!     &          e_multi, ie_surf, isf_4_ele, iele_4_surf,              &
!     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,   &
!     &          num_nod_phys, phys_nod_name, ierr)
!
!      subroutine pvr_main(istep_pvr, numnod, numele, numsurf,          &
!     &         nnod_4_ele, nnod_4_surf, inod_smp_stack, iele_smp_stack,&
!     &         xx, radius, a_radius, s_cylinder, a_s_cylinder, ie,     &
!     &         a_vol_ele, e_multi, ie_surf, isf_4_ele, iele_4_surf,    &
!     &         ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys, &
!     &         istack_nod_component, d_nod)
!
!
      module volume_rendering
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_control_params_4_pvr
      use m_geometry_constants
      use m_pvr_image_array
      use set_default_pvr_params
!
      use m_geometries_in_pvr_screen
      use m_surf_grp_4_pvr_domain
      use m_pvr_ray_startpoints
      use set_position_pvr_screen
      use find_pvr_surf_domain
      use set_pvr_ray_start_point
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_init(numnod, numele, numsurf,                      &
     &          nnod_4_surf, inod_smp_stack, xx,                        &
     &          e_multi, ie_surf, isf_4_ele, iele_4_surf,               &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_nod_phys, phys_nod_name, ierr)
!
      use set_pvr_control
      use cal_pvr_modelview_mat
      use cal_pvr_projection_mat
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: e_multi(numele)
!
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      integer(kind = kint) :: i_pvr
!
!
      call s_set_pvr_control(num_mat, mat_name,                         &
     &    num_nod_phys, phys_nod_name, ierr)
!
      call s_find_pvr_surf_domain(numele, numsurf, e_multi,             &
     &          isf_4_ele, iele_4_surf, num_mat, num_mat_bc,            &
     &          mat_istack, mat_item)
!
      call copy_node_position_for_pvr(numnod, numele,                   &
     &          inod_smp_stack, xx)
      call copy_node_position_pvr_domain(numnod, numele,                &
     &          numsurf, nnod_4_surf, xx, ie_surf, isf_4_ele)
!
      call allocate_nod_data_4_pvr
      call allocate_mesh_outline_pvr
      call allocate_pvr_image_array
      call allocate_num_pvr_ray_start
      call allocate_item_pvr_ray_start
!
!
      do i_pvr = 1, num_pvr
        call cal_mesh_outline_pvr(i_pvr, numnod, xx)
        call check_pvr_parameters(i_pvr)
      end do
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_pixel_on_pvr_screen'
      call set_pixel_on_pvr_screen
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_pvr_modelview_matrix'
      call cal_pvr_modelview_matrix(izero)
      if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_matrix'
      call set_pvr_projection_matrix
!
      if(iflag_rotation .eq.0) then
        do i_pvr = 1, num_pvr
          if(iflag_debug .gt. 0) write(*,*)                             &
     &               'cal_position_pvr_screen', i_pvr
          call cal_position_pvr_screen(modelview_mat(1,i_pvr),          &
     &        projection_mat(1,i_pvr))
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &               'position_pvr_domain_on_screen', i_pvr
          call position_pvr_domain_on_screen(modelview_mat(1,i_pvr),    &
     &        projection_mat(1,i_pvr))
!
          call set_pvr_domain_surface_data(i_pvr, numele, numsurf,      &
   &          nnod_4_surf, ie_surf, isf_4_ele)
        end do
!
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
      end if
!
!      call set_pvr_orthogonal_params
!
      return
!
      end subroutine pvr_init
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_main(istep_pvr, numnod, numele, numsurf,           &
     &         nnod_4_ele, nnod_4_surf, inod_smp_stack, iele_smp_stack, &
     &         xx, radius, a_radius, s_cylinder, a_s_cylinder, ie,      &
     &         a_vol_ele, e_multi, ie_surf, isf_4_ele, iele_4_surf,     &
     &         ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys,  &
     &         istack_nod_component, d_nod)
!
      use cal_pvr_modelview_mat
      use ray_trace_4_each_image
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_surf
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: a_vol_ele(numele)
      real(kind = kreal), intent(in) :: e_multi(numele)
!
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer (kind = kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx(numele,nnod_4_ele,ntot_int_3d,3)
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,num_tot_nod_phys)
!
!
      integer(kind = kint) :: i_pvr
      integer(kind = kint) :: i_rot, ist_rot, ied_rot
!
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_pvr(numnod, numele, nnod_4_ele,                  &
     &          inod_smp_stack, iele_smp_stack, xx, radius,             &
     &          a_radius, s_cylinder, a_s_cylinder, ie, a_vol_ele,      &
     &          ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys, &
     &          istack_nod_component, d_nod)
!
      do i_pvr = 1, num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
        call set_default_pvr_data_params(i_pvr)
!
        if(     iprm_pvr_rot(1,i_pvr).ge.1                              &
     &    .or.  iprm_pvr_rot(1,i_pvr).eq.2                              &
     &    .or.  iprm_pvr_rot(1,i_pvr).eq.3) then
          ist_rot = 1
          ied_rot = iprm_pvr_rot(2,i_pvr)
        else
          ist_rot = 0
          ied_rot = 0
        end if
!
        do i_rot = ist_rot, ied_rot
          if(iflag_rotation .gt. 0) then
            call cal_pvr_modelview_matrix(i_rot)
!
            call cal_position_pvr_screen(modelview_mat(1,i_pvr),        &
     &          projection_mat(1,i_pvr))
            call position_pvr_domain_on_screen(modelview_mat(1,i_pvr),  &
     &          projection_mat(1,i_pvr))
!
            call set_pvr_domain_surface_data(i_pvr, numele, numsurf,    &
   &          nnod_4_surf, ie_surf, isf_4_ele)
          end if
!
          if(iflag_debug .gt. 0) write(*,*) 's_set_pvr_ray_start_point'
          call s_set_pvr_ray_start_point(i_pvr,                         &
     &          numnod, numele, numsurf, nnod_4_surf, xx,               &
     &          ie_surf, isf_4_ele)
!          call check_pvr_ray_startpoints(my_rank)
!
          if(iflag_debug .gt. 0) write(*,*) 's_ray_trace_4_each_image'
          call s_ray_trace_4_each_image(i_pvr, numnod, numele, numsurf, &
     &       nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf, e_multi, xx)
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'blend_pvr_over_domains', i_pvr
          call blend_pvr_over_domains(i_pvr)
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'write_pvr_image_file', i_pvr
          call write_pvr_image_file(i_pvr, i_rot, istep_pvr)
!
          call calypso_MPI_barrier
        end do
!
      end do
!
      end subroutine pvr_main
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering
