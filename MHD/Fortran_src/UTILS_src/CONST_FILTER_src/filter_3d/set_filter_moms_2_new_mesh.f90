!set_filter_moms_2_new_mesh.f90
!      module set_filter_moms_2_new_mesh
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine allocate_iele_local_newfilter
!      subroutine deallocate_iele_local_newfilter
!
!      subroutine set_iele_table_4_newfilter(new_ele)
!
!      subroutine set_new_elength_ele                                   &
!     &         (org_ele, new_node, org_FEM_elen, elen2_ele)
!        type(element_data), intent(in) :: org_ele
!        type(node_data), intent(in) :: new_node
!        type(gradient_model_data_type), intent(in) :: org_FEM_elen
!        type(elen_ele_diffs_type), intent(inout) :: elen2_ele
!      subroutine set_new_filter_moms_ele(org_ele, new_node, FEM_moms, &
!     &          num_filter_moms_2nd, mom2_ele)
!        type(gradient_filter_mom_type), intent(in) :: FEM_moms
!        type(ele_mom_diffs_type), intent(inout)                        &
!       &               :: mom2_ele(num_filter_moms_2nd)
!
      module set_filter_moms_2_new_mesh
!
      use m_precision
!
      implicit none
!
      integer(kind = kint_gl) :: max_gl_ele_newdomain
      integer(kind = kint), allocatable :: iele_local_2nd(:)
!
      integer(kind = kint), parameter :: n_vector = 3
!
      private :: iele_local_2nd, n_vector
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_iele_local_newfilter
!
      allocate(iele_local_2nd(max_gl_ele_newdomain))
      iele_local_2nd(1:max_gl_ele_newdomain) = 0
!
      end subroutine allocate_iele_local_newfilter
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_iele_local_newfilter
!
      deallocate(iele_local_2nd)
!
      end subroutine deallocate_iele_local_newfilter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_iele_table_4_newfilter(new_ele)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: new_ele
!
      integer(kind = kint) :: iele
      integer(kind = kint_gl) :: iele_gl
!
!
      iele_local_2nd(1:max_gl_ele_newdomain) = 0
      do iele = 1, new_ele%numele
        iele_gl = new_ele%iele_global(iele)
        iele_local_2nd(iele_gl) = iele
      end do
!
      end subroutine set_iele_table_4_newfilter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_new_elength_ele                                    &
     &         (org_ele, new_node, org_FEM_elen, elen2_ele)
!
      use t_geometry_data
      use t_filter_elength
!
      type(element_data), intent(in) :: org_ele
      type(node_data), intent(in) :: new_node
      type(gradient_model_data_type), intent(in) :: org_FEM_elen
!
      type(elen_ele_diffs_type), intent(inout) :: elen2_ele
!
      integer(kind = kint) :: iele, iele_2nd, nd
      integer(kind = kint_gl) :: iele_gl
!
!
      do iele = 1, org_ele%numele
        iele_gl = org_ele%iele_global(iele)
        if (org_ele%ie(iele,1) .le. new_node%internal_node              &
     &      .and. iele_gl .le. max_gl_ele_newdomain                     &
     &      .and. iele_local_2nd(iele_gl) .gt. 0) then
!
          iele_2nd = iele_local_2nd(iele_gl)
!
          elen2_ele%moms%f_x2(iele_2nd)                                 &
     &         = org_FEM_elen%elen_ele%moms%f_x2(iele)
          elen2_ele%moms%f_y2(iele_2nd)                                 &
     &         = org_FEM_elen%elen_ele%moms%f_y2(iele)
          elen2_ele%moms%f_z2(iele_2nd)                                 &
     &         = org_FEM_elen%elen_ele%moms%f_z2(iele)
          elen2_ele%moms%f_xy(iele_2nd)                                 &
     &         = org_FEM_elen%elen_ele%moms%f_xy(iele)
          elen2_ele%moms%f_yz(iele_2nd)                                 &
     &         = org_FEM_elen%elen_ele%moms%f_yz(iele)
          elen2_ele%moms%f_zx(iele_2nd)                                 &
     &         = org_FEM_elen%elen_ele%moms%f_zx(iele)
!
          do nd = 1, n_vector
            elen2_ele%diff%df_x2(iele_2nd,nd)                           &
     &         = org_FEM_elen%elen_ele%diff%df_x2(iele,nd)
            elen2_ele%diff%df_y2(iele_2nd,nd)                           &
     &         = org_FEM_elen%elen_ele%diff%df_y2(iele,nd)
            elen2_ele%diff%df_z2(iele_2nd,nd)                           &
     &         = org_FEM_elen%elen_ele%diff%df_z2(iele,nd)
            elen2_ele%diff%df_xy(iele_2nd,nd)                           &
     &         = org_FEM_elen%elen_ele%diff%df_xy(iele,nd)
            elen2_ele%diff%df_yz(iele_2nd,nd)                           &
     &         = org_FEM_elen%elen_ele%diff%df_yz(iele,nd)
            elen2_ele%diff%df_zx(iele_2nd,nd)                           &
     &         = org_FEM_elen%elen_ele%diff%df_zx(iele,nd)
!
            elen2_ele%diff2%df_x2(iele_2nd,nd)                          &
     &         = org_FEM_elen%elen_ele%diff2%df_x2(iele,nd)
            elen2_ele%diff2%df_y2(iele_2nd,nd)                          &
     &         = org_FEM_elen%elen_ele%diff2%df_y2(iele,nd)
            elen2_ele%diff2%df_z2(iele_2nd,nd)                          &
     &         = org_FEM_elen%elen_ele%diff2%df_z2(iele,nd)
            elen2_ele%diff2%df_xy(iele_2nd,nd)                          &
     &         = org_FEM_elen%elen_ele%diff2%df_xy(iele,nd)
            elen2_ele%diff2%df_yz(iele_2nd,nd)                          &
     &         = org_FEM_elen%elen_ele%diff2%df_yz(iele,nd)
            elen2_ele%diff2%df_zx(iele_2nd,nd)                          &
     &         = org_FEM_elen%elen_ele%diff2%df_zx(iele,nd)
          end do
!
        end if
      end do
!
      end subroutine set_new_elength_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_new_filter_moms_ele(org_ele, new_node, FEM_moms,   &
     &          num_filter_moms_2nd, mom2_ele)
!
      use t_geometry_data
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: num_filter_moms_2nd
      type(element_data), intent(in) :: org_ele
      type(node_data), intent(in) :: new_node
      type(gradient_filter_mom_type), intent(in) :: FEM_moms
      type(ele_mom_diffs_type), intent(inout)                           &
     &               :: mom2_ele(num_filter_moms_2nd)
!
      integer(kind = kint) :: iele, iele_2nd, ifil
      integer(kind = kint_gl) :: iele_gl
!
!
      do iele = 1, org_ele%numele
        iele_gl = org_ele%iele_global(iele)
        if (org_ele%ie(iele,1) .le. new_node%internal_node              &
     &      .and. iele_gl .le. max_gl_ele_newdomain                     &
     &      .and. iele_local_2nd(iele_gl) .gt. 0) then
!
          iele_2nd = iele_local_2nd(iele_gl)
!
          if (iele_2nd .gt. size(mom2_ele(1)%moms%f_x2,1) ) then
            write(*,*) 'aho-', iele, iele_gl, iele_2nd
          end if
          if (iele_2nd .le. 0 ) then
            write(*,*) 'baka-', iele, iele_gl, iele_2nd
          end if
!
          do ifil = 1, FEM_moms%num_filter_moms
            call copy_moments_each_point                                &
     &         (iele, FEM_moms%mom_ele(ifil)%moms,                      &
     &          iele_2nd, mom2_ele(ifil)%moms)
            call copy_mom_diffs_each_point                              &
     &         (iele, FEM_moms%mom_ele(ifil)%diff,                      &
     &          iele_2nd, mom2_ele(ifil)%diff)
            call copy_mom_diffs_each_point                              &
     &         (iele, FEM_moms%mom_ele(ifil)%diff2,                     &
     &          iele_2nd, mom2_ele(ifil)%diff2)
          end do
!
        end if
      end do
!
      end subroutine set_new_filter_moms_ele
!
!   --------------------------------------------------------------------
!
      end module set_filter_moms_2_new_mesh
