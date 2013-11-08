!
!     module subroutines_4_search_table
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_target_local_vector(inod, x_target)
!      subroutine copy_position_2_2nd_local_ele(iele, x_local)
!      subroutine cal_3vector_4_tet_2nd(itet, v_target, v_tetra,        &
!     &          x_target, x_local)
!      subroutine init_coefs_on_tet(itet, coefs_by_tet, s)
!      subroutine check_solution_in_tet(ref_error, s_coef)
!      subroutine set_results_2_array(n_rank_org, inod, jele, xi)
!      subroutine set_results_2_array_fin(inod, jele, xi,               &
!     &          differ_tmp, differ_res, iflag_org_tmp)
!      subroutine check_missing_nodes(ierr, my_rank)
!
      module subroutines_4_search_table
!
      use m_precision
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
      subroutine copy_target_local_vector(inod, x_target)
!
      use m_geometry_data
!
      integer(kind = kint), intent(in) :: inod
      real(kind = kreal), intent(inout) :: x_target(3)
!
      x_target(1:3) = xx(inod,1:3)
!
      end subroutine copy_target_local_vector
!
!-----------------------------------------------------------------------
!
      subroutine copy_position_2_2nd_local_ele(iele, x_local)
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout) :: x_local(nnod_4_ele_2nd,3)
!
      integer(kind = kint) :: i, inod
!
!
       do i = 1, nnod_4_ele_2nd
         inod = ie_2nd(iele,i)
         x_local(i,1:3) = xx_2nd(inod,1:3)
       end do
!
      end subroutine copy_position_2_2nd_local_ele
!
!-----------------------------------------------------------------------
!
      subroutine cal_3vector_4_tet_2nd(itet, v_target, v_tetra,         &
     &          x_target, x_local)
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_connect_hexa_2_tetra
!
      integer(kind = kint), intent(in) :: itet
      real(kind = kreal), intent(in) :: x_target(3)
      real(kind = kreal), intent(in) :: x_local(nnod_4_ele_2nd,3)
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
      subroutine init_coefs_on_tet(itet, coefs_by_tet, s)
!
      use m_constants
      use m_2nd_geometry_param
      use m_connect_hexa_2_tetra
!
      integer(kind = kint), intent(in) :: itet
      real(kind = kreal), intent(in) ::    s(3)
      real(kind = kreal), intent(inout) :: coefs_by_tet(nnod_4_ele_2nd)
!
      integer(kind = kint) :: i1, i2, i3, i4
!
!
        coefs_by_tet(1:nnod_4_ele_2nd) = zero
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
      subroutine set_results_2_array(n_rank_org, inod, jele, xi)
!
      use m_ctl_params_4_gen_table
      use m_interpolate_table_dest
      use m_interpolate_coefs_dest
      use m_work_const_itp_table
!
      integer(kind = kint), intent(in) :: n_rank_org, inod, jele
      real(kind = kreal), intent(inout) ::   xi(3)
!
      integer(kind = kint) :: nd
      real(kind= kreal), parameter :: one = 1.0d0
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
      iele_org_4_dest(inod) =  jele
      coef_inter_dest(inod,1:3) = xi(1:3)
!
      end subroutine set_results_2_array
!
!-----------------------------------------------------------------------
!
      subroutine set_results_2_array_fin(n_rank_org, inod, jele, xi,    &
     &          differ_tmp, differ_res, iflag_org_tmp)
!
      use m_constants
      use m_ctl_params_4_gen_table
      use m_interpolate_table_dest
      use m_interpolate_coefs_dest
!
      integer(kind = kint), intent(in) :: n_rank_org, inod, jele
      real(kind = kreal), intent(in) ::  differ_res
      integer(kind = kint), intent(inout) :: iflag_org_tmp
      real(kind = kreal), intent(inout) :: xi(3), differ_tmp
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
      iele_org_4_dest(inod) =  jele
      coef_inter_dest(inod,1:3) = xi(1:3)
      differ_tmp = differ_res
!
      end subroutine set_results_2_array_fin
!
!-----------------------------------------------------------------------
!
      subroutine check_missing_nodes(ierr, my_rank)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_work_const_itp_table
      use m_sphere_bin_4_table
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) ::ierr
!
      integer(kind= kint), parameter :: id_miss_file = 12
      character(len=kchara) :: miss_file_name
      integer(kind = kint) :: inod
!
!
      call add_int_suffix(my_rank, miss_file_head, miss_file_name)
      open (id_miss_file, file = miss_file_name)
!
      ierr = 0
      write(id_miss_file,*) 'missing nodes: '
      do inod = 1, internal_node
        if (iflag_org_domain(inod) .le. 0) then
          ierr = ierr + 1
          write(id_miss_file,'(i10,1p3e16.7,3i10)')                     &
     &              inod, xx(inod,1:3), id_search_area(inod,1:3)
        end if
      end do
      close(id_miss_file)
!
      write(*,*) 'Number of missing nodes: ', ierr
!
      end subroutine check_missing_nodes
!
!-----------------------------------------------------------------------
!
      end module subroutines_4_search_table
