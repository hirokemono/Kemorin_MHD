!sleeve_edge_side_cube.f90
!     module sleeve_edge_side_cube
!
!      Written by Kemorin
!
!!      subroutine set_sleeve_edge_xmin(c_size, c_vert, nb_rng, sl_rng, &
!!     &          kpe, jnp, knp, ioff_gl, nd, loc_id, node, inod)
!!      subroutine set_sleeve_edge_xmax(c_size, c_vert, nb_rng, sl_rng, &
!!     &          kpe, jnp, knp, ioff_gl, nd, loc_id, node, inod)
!!      subroutine set_sleeve_edge_ymin(c_size, c_vert, nb_rng, sl_rng, &
!!     &          kpe, inp, knp, ioff_gl, nd, loc_id, node, inod)
!!      subroutine set_sleeve_edge_ymax(c_size, c_vert, nb_rng, sl_rng, &
!!     &          kpe, inp, knp, ioff_gl, nd, loc_id, node, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(vertical_position_cube), intent(in) :: c_vert
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(slleve_range), intent(in) :: sl_rng
!!        type(local_node_id_cube), intent(inout) :: loc_id
!!        type(node_data), intent(inout) :: node
!
      module sleeve_edge_side_cube
!
      use m_precision
      use m_constants
!
      use t_size_of_cube
      use t_sleeve_cube
      use t_neib_range_cube
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
      subroutine set_sleeve_edge_xmin(c_size, c_vert, nb_rng, sl_rng,   &
     &          kpe, jnp, knp, ioff_gl, nd, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: kpe, jnp, knp
      integer (kind = kint), intent(in) :: nd
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      type(slleve_range) :: sl_rng_2
      integer (kind = kint) :: i, j, k
!
!
       call copy_slleve_size(sl_rng, sl_rng_2)
       if ( nd.eq.2 .and. jnp.gt.0) sl_rng_2%je = sl_rng_2%je - 1
       if ( nd.eq.3 .and. knp.gt.0) sl_rng_2%ke = sl_rng_2%ke - 1
       if ( nd.eq.3 .and. kpe.eq.c_size%ndz .and. knp.eq.0 )            &
     &                              sl_rng_2%ke = sl_rng_2%ke-1

       do k = sl_rng_2%ks, sl_rng_2%ke
        do j = sl_rng_2%js, sl_rng_2%je
         do i = sl_rng_2%is, sl_rng_2%ie

          inod = inod + 1

          loc_id%edge_id_lc(i,j,k,nd) =  inod
          node%inod_global(inod) = ioff_gl + i                          &
     &            + (nb_rng%joff + j) * (sl_rng_2%ie - sl_rng_2%is + 1) &
     &            + (nb_rng%koff + k-1)*(sl_rng_2%ie - sl_rng_2%is + 1) &
     &             * c_size%ny_all

          if (nd .eq. 1) then
           node%xx(inod,1) = c_size%xmin + ( dble(i)-half )             &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = nb_rng%yoff + dble(j-1)                    &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          else if (nd .eq. 2) then
           node%xx(inod,1) = c_size%xmin + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = nb_rng%yoff + (dble(j) - half)             &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          else if (nd .eq. 3) then
           node%xx(inod,1) = c_size%xmin + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = nb_rng%yoff + dble(j-1)                    &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz_edge(nb_rng%koff + k)
          end if
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_xmax(c_size, c_vert, nb_rng, sl_rng,   &
     &          kpe, jnp, knp, ioff_gl, nd, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: kpe, jnp, knp
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: nd
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      type(slleve_range) :: sl_rng_2
      integer (kind = kint) :: i, j, k, i1
!
!
       call copy_slleve_size(sl_rng, sl_rng_2)
       if ( nd.eq.1 ) sl_rng_2%ie = sl_rng_2%ie - 1
       if ( nd.eq.2 .and. jnp.gt.0) sl_rng_2%je = sl_rng_2%je - 1
       if ( nd.eq.3 .and. knp.gt.0) sl_rng_2%ke = sl_rng_2%ke-1
       if ( nd.eq.3 .and. kpe.eq.c_size%ndz .and. knp.eq.0 )            &
     &                              sl_rng_2%ke = sl_rng_2%ke-1

       do k = sl_rng_2%ks, sl_rng_2%ke
        do j = sl_rng_2%js, sl_rng_2%je
         do i = sl_rng_2%is, sl_rng_2%ie

          inod = inod + 1

          i1 = c_size%nxi + c_size%ndepth + i
          loc_id%edge_id_lc(i1,j,k,nd) =  inod
          node%inod_global(inod) = ioff_gl + i                          &
     &          + (nb_rng%joff + j) * (sl_rng_2%ie - sl_rng_2%is + 1)   &
     &          + (nb_rng%koff + k-1) * (sl_rng_2%ie - sl_rng_2%is + 1) &
     &           * c_size%ny_all

          if (nd .eq. 1) then
           node%xx(inod,1) = c_size%xmax + (dble(i+c_size%ndepth)-half) &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = nb_rng%yoff + dble(j-1)                    &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          else if (nd .eq. 2) then
           node%xx(inod,1) = c_size%xmax + dble(i+c_size%ndepth-1)      &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = nb_rng%yoff + (dble(j)-half)               &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          else if (nd .eq. 3) then
           node%xx(inod,1) = c_size%xmax + dble(i+c_size%ndepth-1)      &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = nb_rng%yoff + dble(j-1)                    &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz_edge(nb_rng%koff + k)
          end if
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmax
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_ymin(c_size, c_vert, nb_rng, sl_rng,   &
     &          kpe, inp, knp, ioff_gl, nd, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: kpe, inp, knp
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: nd
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      type(slleve_range) :: sl_rng_2
      integer (kind= kint) :: i, j, k
!
!
       call copy_slleve_size(sl_rng, sl_rng_2)
       if ( nd.eq.1 .and. inp.gt.0) sl_rng_2%ie = sl_rng_2%ie - 1
       if ( nd.eq.3 .and. knp.gt.0) sl_rng_2%ke = sl_rng_2%ke - 1
       if ( nd.eq.3 .and. kpe.eq.c_size%ndz .and. knp.eq.0)             &
     &                              sl_rng_2%ke = sl_rng_2%ke - 1

       do k = sl_rng_2%ks, sl_rng_2%ke
        do j = sl_rng_2%js, sl_rng_2%je
         do i = sl_rng_2%is, sl_rng_2%ie

          inod = inod + 1

          loc_id%edge_id_lc(i,j,k,nd) =  inod
          node%inod_global(inod)                                        &
     &        = ioff_gl + (nb_rng%ioff + i) + (j-1)*c_size%nx_all       &
     &            + (nb_rng%koff + k-1)*(sl_rng_2%je - sl_rng_2%js + 1) &
     &             * c_size%nx_all

          if (nd .eq. 1) then
           node%xx(inod,1) = nb_rng%xoff + (dble(i) - half)             &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymin + dble(j-1)                    &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          else if (nd .eq. 2) then
           node%xx(inod,1) = nb_rng%xoff + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymin + ( dble(j)-half )             &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          else if (nd .eq. 3) then
           node%xx(inod,1) = nb_rng%xoff + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymin + dble(j-1)                    &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz_edge(nb_rng%koff + k)
          end if
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_ymax(c_size, c_vert, nb_rng, sl_rng,   &
     &          kpe, inp, knp, ioff_gl, nd, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: kpe, inp, knp
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: nd
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      type(slleve_range) :: sl_rng_2
      integer (kind = kint) :: i, j, k, j1
!
!
       call copy_slleve_size(sl_rng, sl_rng_2)
       if ( nd.eq.1 .and. inp.gt.0) sl_rng_2%ie = sl_rng_2%ie - 1
       if ( nd.eq.2)                sl_rng_2%je = sl_rng_2%je-1
       if ( nd.eq.3 .and. knp.gt.0) sl_rng_2%ke = sl_rng_2%ke - 1
       if ( nd.eq.3 .and. kpe.eq.c_size%ndz .and. knp.eq.0)             &
     &                              sl_rng_2%ke = sl_rng_2%ke-1

       do k = sl_rng_2%ks, sl_rng_2%ke
        do j = sl_rng_2%js, sl_rng_2%je
         do i = sl_rng_2%is, sl_rng_2%ie

          inod = inod + 1

          j1 = c_size%nyi + c_size%ndepth + j
          loc_id%edge_id_lc(i,j1,k,nd) =  inod
          node%inod_global(inod)                                        &
     &        = ioff_gl + (nb_rng%ioff + i) + (j-1)*c_size%nx_all       &
     &            + (nb_rng%koff + k-1)*(sl_rng_2%je - sl_rng_2%js + 1) &
     &             * c_size%nx_all

          if (nd .eq. 1) then
           node%xx(inod,1) = nb_rng%xoff + (dble(i) - half)             &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymax + dble(c_size%ndepth+j-1)      &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          else if (nd .eq. 2) then
           node%xx(inod,1) = nb_rng%xoff + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymax + (dble(c_size%ndepth+j)-half) &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(nb_rng%koff + k)
          else if (nd .eq. 3) then
           node%xx(inod,1) = nb_rng%xoff + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymax + dble(c_size%ndepth+j-1)      &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz_edge(nb_rng%koff + k)
          end if
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_ymax
!
!  ---------------------------------------------------------------------
!
      end module sleeve_edge_side_cube
