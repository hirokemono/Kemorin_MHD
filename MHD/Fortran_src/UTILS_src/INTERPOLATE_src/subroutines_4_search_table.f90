!
!     module subroutines_4_search_table
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_target_local_vector(node, inod, x_target)
!      subroutine copy_position_2_2nd_local_ele(new_node, new_ele,      &
!     &          iele, x_local)
!      subroutine cal_3vector_4_tet_2nd(nnod_4_ele_2, itet,             &
!     &          v_target, v_tetra, x_target, x_local)
!      subroutine init_coefs_on_tet(nnod_4_ele_2, itet, coefs_by_tet, s)
!      subroutine check_solution_in_tet(ref_error, s_coef)
!      subroutine set_results_2_array(n_rank_org, inod, jele, xi,       &
!     &          itp_coef_dest)
!      subroutine set_results_2_array_fin(inod, jele, xi,               &
!     &          differ_tmp, differ_res, iflag_org_tmp, itp_coef_dest)
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!      subroutine check_missing_nodes(ierr, id_rank, node)
!
      module subroutines_4_search_table
!
      use m_precision
      use m_constants
!
      implicit none
!
      character(len=kchara), private, parameter                         &
     &                      :: miss_file_head = 'missing_node_list'
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_target_local_vector(node, inod, x_target)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: inod
      real(kind = kreal), intent(inout) :: x_target(3)
!
      x_target(1:3) = node%xx(inod,1:3)
!
      end subroutine copy_target_local_vector
!
!-----------------------------------------------------------------------
!
      subroutine copy_position_2_2nd_local_ele(new_node, new_ele,       &
     &          iele, x_local)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: iele
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
      real(kind = kreal), intent(inout)                                 &
     &       :: x_local(new_ele%nnod_4_ele,3)
!
      integer(kind = kint) :: i, inod
!
!
       do i = 1, new_ele%nnod_4_ele
         inod = new_ele%ie(iele,i)
         x_local(i,1:3) = new_node%xx(inod,1:3)
       end do
!
      end subroutine copy_position_2_2nd_local_ele
!
!-----------------------------------------------------------------------
!
      subroutine cal_3vector_4_tet_2nd(nnod_4_ele_2, itet,              &
     &          v_target, v_tetra, x_target, x_local)
!
      use m_connect_hexa_2_tetra
!
      integer(kind = kint), intent(in) :: itet, nnod_4_ele_2
      real(kind = kreal), intent(in) :: x_target(3)
      real(kind = kreal), intent(in) :: x_local(nnod_4_ele_2,3)
!
      real(kind = kreal), intent(inout) :: v_target(3)
      real(kind = kreal), intent(inout) :: v_tetra(3,3)
!
      integer(kind = kint) :: nd
!
!
       do nd = 1, 3
!
         v_target(nd) =  x_target(nd)                                   &
     &                    - x_local(ie_tetra(1,itet),nd)
!
         v_tetra(nd,1) = x_local(ie_tetra(2,itet),nd)                   &
     &                    - x_local(ie_tetra(1,itet),nd)
         v_tetra(nd,2) = x_local(ie_tetra(3,itet),nd)                   &
     &                    - x_local(ie_tetra(1,itet),nd)
         v_tetra(nd,3) = x_local(ie_tetra(4,itet),nd)                   &
     &                    - x_local(ie_tetra(1,itet),nd)
       end do
!
      end subroutine cal_3vector_4_tet_2nd
!
!-----------------------------------------------------------------------
!
      subroutine init_coefs_on_tet(nnod_4_ele_2, itet, coefs_by_tet, s)
!
      use m_constants
      use m_connect_hexa_2_tetra
!
      integer(kind = kint), intent(in) :: itet, nnod_4_ele_2
      real(kind = kreal), intent(in) ::    s(3)
      real(kind = kreal), intent(inout) :: coefs_by_tet(nnod_4_ele_2)
!
      integer(kind = kint) :: i1, i2, i3, i4
!
!
        coefs_by_tet(1:nnod_4_ele_2) = zero
!
        i1 = ie_tetra(1,itet)
        i2 = ie_tetra(2,itet)
        i3 = ie_tetra(3,itet)
        i4 = ie_tetra(4,itet)
!
        coefs_by_tet(i1) = one - ( s(1) + s(2) + s(3) )
        coefs_by_tet(i2) = s(1)
        coefs_by_tet(i3) = s(2)
        coefs_by_tet(i4) = s(3)
!
      end subroutine init_coefs_on_tet
!
!-----------------------------------------------------------------------
!
      subroutine check_solution_in_tet(ref_error, s_coef)
!
      use m_constants
!
      real(kind = kreal), intent(in) ::    s_coef(3)
      real(kind = kreal), intent(inout) :: ref_error
!
      real(kind = kreal) :: sum_tmp
      integer (kind = kint) :: nd
!
!
       sum_tmp = s_coef(1) + s_coef(2) + s_coef(3)
!
       ref_error = zero
       do nd = 1, 3
         if ( s_coef(nd) .lt. zero) then
           ref_error = ref_error - s_coef(nd)
         else if ( s_coef(nd) .gt. one) then
           ref_error = ref_error + s_coef(nd) - one
         end if
       end do
!
       if ( sum_tmp .lt. zero) then
         ref_error = ref_error - sum_tmp
       else if ( sum_tmp .gt. one) then
         ref_error = ref_error + sum_tmp - one
       end if
!
      end subroutine check_solution_in_tet
!
!-----------------------------------------------------------------------
!
      subroutine set_results_2_array                                    &
     &         (n_rank_org, inod, jele, xi, itp_coef_dest)
!
      use m_ctl_params_4_gen_table
      use t_interpolate_coefs_dest
      use m_work_const_itp_table
!
      integer(kind = kint), intent(in) :: n_rank_org, inod, jele
!
      real(kind = kreal), intent(inout) ::   xi(3)
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        if ( abs(xi(nd) - one) .lt. eps_iter) then
          xi(nd) = one
        else if ( abs(xi(nd) + one) .lt. eps_iter) then
          xi(nd) = -one
        end if
      end do

!
      iflag_org_domain(inod) = n_rank_org + 1
      itp_coef_dest%iele_org_4_dest(inod) =  jele
      itp_coef_dest%coef_inter_dest(inod,1:3) = xi(1:3)
!
      end subroutine set_results_2_array
!
!-----------------------------------------------------------------------
!
      subroutine set_results_2_array_fin(n_rank_org, inod, jele, xi,    &
     &          differ_tmp, differ_res, iflag_org_tmp, itp_coef_dest)
!
      use m_constants
      use m_ctl_params_4_gen_table
      use t_interpolate_coefs_dest
!
      integer(kind = kint), intent(in) :: n_rank_org, inod, jele
      real(kind = kreal), intent(in) ::  differ_res
!
      integer(kind = kint), intent(inout) :: iflag_org_tmp
      real(kind = kreal), intent(inout) :: xi(3), differ_tmp
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        if ( xi(nd) .gt. one) then
          xi(nd) = one
        else if ( xi(nd) .lt. -one) then
          xi(nd) = -one
        end if
      end do
!
      iflag_org_tmp = n_rank_org + 1
      itp_coef_dest%iele_org_4_dest(inod) =  jele
      itp_coef_dest%coef_inter_dest(inod,1:3) = xi(1:3)
      differ_tmp = differ_res
!
      end subroutine set_results_2_array_fin
!
!-----------------------------------------------------------------------
!
      subroutine check_missing_nodes(ierr, id_rank, node)
!
      use t_geometry_data
      use m_work_const_itp_table
      use m_search_bolck_4_itp
!
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(inout) ::ierr
      type(node_data), intent(in) :: node
!
      integer(kind= kint), parameter :: id_miss_file = 12
      character(len=kchara) :: miss_file_name
      integer(kind = kint) :: inod
!
!
      miss_file_name = add_process_id(id_rank, miss_file_head)
      open (id_miss_file, file = miss_file_name)
!
      ierr = 0
      write(id_miss_file,*) 'missing nodes: '
      do inod = 1, node%internal_node
        if (iflag_org_domain(inod) .le. 0) then
          ierr = ierr + 1
          write(id_miss_file,'(i16,1p3e16.7)') inod, node%xx(inod,1:3)
        end if
      end do
      close(id_miss_file)
!
      write(*,*) 'Number of missing nodes: ', ierr,                     &
     &          ' of  ', node%internal_node, ' at rank ', id_rank
!
      end subroutine check_missing_nodes
!
!-----------------------------------------------------------------------
!
      end module subroutines_4_search_table
