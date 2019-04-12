!sleeve_edge_side_cube.f90
!     module sleeve_edge_side_cube
!
!      Written by Kemorin
!
!!       subroutine set_sleeve_edge_xmin(sl_rng, kpe, jnp, knp,         &
!!     &          inod, ioff_gl, nd)
!!       subroutine set_sleeve_edge_xmax(sl_rng, kpe, jnp, knp,         &
!!     &          inod, ioff_gl, nd)
!!       subroutine set_sleeve_edge_ymin(sl_rng, kpe, inp, knp,         &
!!     &          inod, ioff_gl, nd)
!!       subroutine set_sleeve_edge_ymax(sl_rng, kpe, inp, knp,         &
!!     &          inod, ioff_gl, nd)
!!        type(slleve_range), intent(in) :: sl_rng
!
      module sleeve_edge_side_cube
!
      use m_precision
      use m_constants
!
      use t_sleeve_cube
      use m_size_of_cube
      use m_size_4_plane
      use m_offset_size_cube
      use m_cube_position
      use m_local_node_id_cube
      use m_cube_files_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine set_sleeve_edge_xmin(sl_rng, kpe, jnp, knp,           &
     &          inod, ioff_gl, nd)
!
      type(slleve_range), intent(in) :: sl_rng
       integer (kind = kint), intent(in) :: ioff_gl
       integer (kind = kint), intent(in) :: kpe, jnp, knp
       integer (kind = kint), intent(in) :: nd
       integer (kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng_2
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) :: i, j, k
       real (kind = kreal) :: x, y, z
!
!
       call copy_slleve_size(sl_rng, sl_rng_2)
       if ( nd.eq.2 .and. jnp.gt.0) sl_rng_2%je = sl_rng_2%je - 1
       if ( nd.eq.3 .and. knp.gt.0) sl_rng_2%ke = sl_rng_2%ke - 1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 )                   &
     &                              sl_rng_2%ke = sl_rng_2%ke-1

       do k = sl_rng_2%ks, sl_rng_2%ke
        do j = sl_rng_2%js, sl_rng_2%je
         do i = sl_rng_2%is, sl_rng_2%ie

          inod = inod + 1

          edge_id_lc(i,j,k,nd) =  inod
          node_id_gl = ioff_gl + i                                      &
     &                + (joff+j) * (sl_rng_2%ie - sl_rng_2%is + 1)      &
     &                + (koff+k-1)*(sl_rng_2%ie - sl_rng_2%is + 1)      &
     &                            * ny_all

          if (nd .eq. 1) then
           x = xmin + ( dble(i)-half )*xsize / dble(nx_all)
           y = yoff + dble(j-1) *      ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmin + dble(i-1) *      xsize / dble(nx_all)
           y = yoff + ( dble(j)-half )*ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmin + dble(i-1) *      xsize / dble(nx_all)
           y = yoff + dble(j-1) *      ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
!
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmin
!
!  ---------------------------------------------------------------------
!
       subroutine set_sleeve_edge_xmax(sl_rng, kpe, jnp, knp,           &
     &          inod, ioff_gl, nd)
!
      type(slleve_range), intent(in) :: sl_rng
       integer (kind = kint), intent(in) :: kpe, jnp, knp
       integer (kind = kint), intent(in) :: ioff_gl
       integer (kind = kint), intent(in) :: nd
       integer (kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng_2
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) :: i, j, k
       real (kind = kreal) :: x, y, z
!
!
       call copy_slleve_size(sl_rng, sl_rng_2)
       if ( nd.eq.1 ) sl_rng_2%ie = sl_rng_2%ie - 1
       if ( nd.eq.2 .and. jnp.gt.0) sl_rng_2%je = sl_rng_2%je - 1
       if ( nd.eq.3 .and. knp.gt.0) sl_rng_2%ke = sl_rng_2%ke-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 )                   &
     &                              sl_rng_2%ke = sl_rng_2%ke-1

       do k = sl_rng_2%ks, sl_rng_2%ke
        do j = sl_rng_2%js, sl_rng_2%je
         do i = sl_rng_2%is, sl_rng_2%ie

          inod = inod + 1

          edge_id_lc(nxi+ndepth+i,j,k,nd) =  inod
          node_id_gl = ioff_gl + i                                      &
     &                + (joff+j) * (sl_rng_2%ie - sl_rng_2%is + 1)      &
     &                + (koff+k-1) * (sl_rng_2%ie - sl_rng_2%is + 1)    &
     &                             * ny_all

          if (nd .eq. 1) then
           x = xmax + ( dble(i+ndepth)-half )*xsize / dble(nx_all)
           y = yoff + dble(j-1) *             ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmax + dble(i+ndepth-1) *     xsize / dble(nx_all)
           y = yoff + (dble(j)-half) *     ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmax + dble(i+ndepth-1) *     xsize / dble(nx_all)
           y = yoff + dble(j-1) *            ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
!
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmax
!
!  ---------------------------------------------------------------------
!
       subroutine set_sleeve_edge_ymin(sl_rng, kpe, inp, knp,           &
     &          inod, ioff_gl, nd)
!
      type(slleve_range), intent(in) :: sl_rng
       integer (kind = kint), intent(in) :: kpe, inp, knp
       integer (kind = kint), intent(in) :: ioff_gl
       integer (kind = kint), intent(in) :: nd
       integer (kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng_2
       integer (kind= kint) :: node_id_gl
       integer (kind= kint) :: i, j, k
       real (kind= kreal) :: x, y, z
!
!
       call copy_slleve_size(sl_rng, sl_rng_2)
       if ( nd.eq.1 .and. inp.gt.0) sl_rng_2%ie = sl_rng_2%ie - 1
       if ( nd.eq.3 .and. knp.gt.0) sl_rng_2%ke = sl_rng_2%ke - 1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0)                    &
     &                              sl_rng_2%ke = sl_rng_2%ke - 1

       do k = sl_rng_2%ks, sl_rng_2%ke
        do j = sl_rng_2%js, sl_rng_2%je
         do i = sl_rng_2%is, sl_rng_2%ie

          inod = inod + 1

          edge_id_lc(i,j,k,nd) =  inod
          node_id_gl = ioff_gl + (ioff+i) + (j-1)*nx_all                &
     &             + (koff+k-1)*(sl_rng_2%je - sl_rng_2%js + 1)*nx_all

          if (nd .eq. 1) then
           x = xoff + ( dble(i)-half )*xsize / dble(nx_all)
           y = ymin + dble(j-1) *        ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xoff + dble(i-1) *        xsize / dble(nx_all)
           y = ymin + ( dble(j)-half ) *   ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xoff + dble(i-1) *        xsize / dble(nx_all)
           y = ymin + dble(j-1) *        ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
!
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_ymin
!
!  ---------------------------------------------------------------------
!
       subroutine set_sleeve_edge_ymax(sl_rng, kpe, inp, knp,           &
     &          inod, ioff_gl, nd)
!
      type(slleve_range), intent(in) :: sl_rng
       integer (kind = kint), intent(in) :: kpe, inp, knp
       integer (kind = kint), intent(in) :: ioff_gl
       integer (kind = kint), intent(in) :: nd
       integer (kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng_2
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) :: i, j, k
       real (kind = kreal) :: x, y, z
!
!
       call copy_slleve_size(sl_rng, sl_rng_2)
       if ( nd.eq.1 .and. inp.gt.0) sl_rng_2%ie = sl_rng_2%ie - 1
       if ( nd.eq.2)                sl_rng_2%je = sl_rng_2%je-1
       if ( nd.eq.3 .and. knp.gt.0) sl_rng_2%ke = sl_rng_2%ke - 1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0)                    &
     &                              sl_rng_2%ke = sl_rng_2%ke-1

       do k = sl_rng_2%ks, sl_rng_2%ke
        do j = sl_rng_2%js, sl_rng_2%je
         do i = sl_rng_2%is, sl_rng_2%ie

          inod = inod + 1

          edge_id_lc(i,nyi+ndepth+j,k,nd) =  inod
          node_id_gl = ioff_gl + (ioff+i) + (j-1)*nx_all                &
     &              + (koff+k-1)*(sl_rng_2%je - sl_rng_2%js + 1)*nx_all

          if (nd .eq. 1) then
           x = xoff + ( dble(i)-half )*         xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *          ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xoff + dble(i-1) *               xsize / dble(nx_all)
           y = ymax + ( dble(ndepth+j)-half ) * ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xoff + dble(i-1) *               xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
!
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_ymax
!
!  ---------------------------------------------------------------------
!
      end module sleeve_edge_side_cube
