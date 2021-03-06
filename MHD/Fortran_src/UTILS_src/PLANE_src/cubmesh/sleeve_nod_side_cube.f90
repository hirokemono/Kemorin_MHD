!
!     module sleeve_nod_side_cube
!
!      Written by Kemorin
!
!!      subroutine set_sleeve_node_xmin(c_size, c_vert, nb_rng, sl_rng, &
!!     &          ioff_gl, loc_id, node, inod)
!!      subroutine set_sleeve_node_xmax(c_size, c_vert, nb_rng, sl_rng, &
!!     &          ioff_gl, loc_id, node, inod)
!!      subroutine set_sleeve_node_ymin(c_size, c_vert, nb_rng, sl_rng, &
!!     &          ioff_gl, loc_id, node, inod)
!!      subroutine set_sleeve_node_ymax(c_size, c_vert, nb_rng, sl_rng, &
!!     &          ioff_gl, loc_id, node, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(vertical_position_cube), intent(in) :: c_vert
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(slleve_range), intent(in) :: sl_rng
!!        type(local_node_id_cube), intent(inout) :: loc_id
!!        type(node_data), intent(inout) :: node
!
      module sleeve_nod_side_cube
!
      use m_precision
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
      use t_cube_position
      use t_local_node_id_cube
      use t_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_xmin(c_size, c_vert, nb_rng, sl_rng,   &
     &          ioff_gl, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      integer (kind= kint) :: i, j, k
!
!
      do k = sl_rng%ks, sl_rng%ke
        do j = sl_rng%js, sl_rng%je
          do i = 1, c_size%ndepth
            inod = inod + 1

            loc_id%node_id_lc(i,j,k) =  inod
            node%inod_global(inod) = ioff_gl                            &
     &                  + i + (j + nb_rng%joff-1) * c_size%ndepth       &
     &                  + (k + nb_rng%koff-1) * c_size%ndepth           &
     &                   * c_size%ny_all

            node%xx(inod,1) = c_size%xmin + (i-1)                       &
     &                       * c_size%xsize / dble(c_size%nx_all)
            node%xx(inod,2) = nb_rng%yoff + (j-1)                       &
     &                       * c_size%ysize / dble(c_size%ny_all)
            node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          end do
        end do
      end do
!
      end subroutine set_sleeve_node_xmin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_xmax(c_size, c_vert, nb_rng, sl_rng,   &
     &          ioff_gl, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      integer (kind= kint) :: i, j, k, i1
!
!
      do k = sl_rng%ks, sl_rng%ke
        do j = sl_rng%js, sl_rng%je
          do i = 1, c_size%ndepth
            inod = inod + 1

            i1 = c_size%nxi + c_size%ndepth + i
            loc_id%node_id_lc(i1,j,k) =  inod
            node%inod_global(inod) = ioff_gl                            &
     &                  + i + (j + nb_rng%joff-1) * c_size%ndepth       &
     &                  + (k + nb_rng%koff-1) * c_size%ndepth           &
     &                                        * c_size%ny_all

            node%xx(inod,1) = c_size%xmax + (i+c_size%ndepth-1)         &
     &                       * c_size%xsize / dble(c_size%nx_all)
            node%xx(inod,2) = nb_rng%yoff + (j-1)                       &
     &                       * c_size%ysize / dble(c_size%ny_all)
            node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          end do
        end do
      end do
!
      end subroutine set_sleeve_node_xmax
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_ymin(c_size, c_vert, nb_rng, sl_rng,   &
     &          ioff_gl, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      integer (kind= kint) :: i, j, k
!
!
      do k = sl_rng%ks, sl_rng%ke
        do j = 1, c_size%ndepth
          do i = sl_rng%is, sl_rng%ie
            inod = inod + 1

            loc_id%node_id_lc(i,j,k) =  inod
            node%inod_global(inod) = ioff_gl                            &
     &                  + (nb_rng%ioff + i) + (j-1) * c_size%nx_all     &
     &                  + (nb_rng%koff + k-1) * c_size%nx_all           &
     &                   * c_size%ndepth

            node%xx(inod,1) = nb_rng%xoff + (i-1)                       &
     &                     * c_size%xsize / dble(c_size%nx_all)
            node%xx(inod,2) = c_size%ymin + (j-1)                       &
     &                     * c_size%ysize / dble(c_size%ny_all)
            node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          end do
        end do
      end do
!
      end subroutine set_sleeve_node_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_ymax(c_size, c_vert, nb_rng, sl_rng,   &
     &          ioff_gl, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer(kind = kint), intent(in) :: ioff_gl
!
      integer(kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      integer (kind= kint) :: i, j, k, j1
!
!
      do k = sl_rng%ks, sl_rng%ke
        do j = 1, c_size%ndepth
          do i = sl_rng%is, sl_rng%ie
            inod = inod + 1
            j1 = c_size%nyi + c_size%ndepth + j

            loc_id%node_id_lc(i,j1,k) =  inod
            node%inod_global(inod) = ioff_gl                            &
     &               + (nb_rng%ioff + i) + (j-1) * c_size%nx_all        &
     &               + (nb_rng%koff + k-1) * c_size%nx_all              &
     &                * c_size%ndepth 

            node%xx(inod,1) = nb_rng%xoff + (i-1)                       &
     &                       * c_size%xsize / dble(c_size%nx_all)
            node%xx(inod,2) = c_size%ymax + (j+c_size%ndepth-1)         &
     &                       * c_size%ysize / dble(c_size%ny_all)
            node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          end do
        end do
      end do
!
      end subroutine set_sleeve_node_ymax
!
!  ---------------------------------------------------------------------
!
      end module sleeve_nod_side_cube
