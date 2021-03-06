!sleeve_edge_corner_cube.f90
!     module sleeve_edge_corner_cube
!
!      Written by Kemorin
!
!!      subroutine set_sleeve_edge_xmin_ymin(c_size, c_vert, sl_rng,    &
!!     &          kpe, knp, ioff_gl, koff, nd, loc_id, node, inod)
!!      subroutine set_sleeve_edge_xmax_ymin(c_size, c_vert, sl_rng,    &
!!     &          kpe, knp, ioff_gl, koff, nd, loc_id, node, inod)
!!      subroutine set_sleeve_edge_xmax_ymax(c_size, c_vert, sl_rng,    &
!!     &          kpe, knp, ioff_gl, koff, nd, loc_id, node, inod)
!!      subroutine set_sleeve_edge_xmin_ymax(c_size, c_vert, sl_rng,    &
!!     &          kpe, knp, ioff_gl, koff, nd, loc_id, node, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(vertical_position_cube), intent(in) :: c_vert
!!        type(slleve_range), intent(in) :: sl_rng
!!        type(local_node_id_cube), intent(inout) :: loc_id
!!        type(node_data), intent(inout) :: node
!
      module sleeve_edge_corner_cube
!
      use m_precision
      use m_constants
!
      use t_size_of_cube
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
      subroutine set_sleeve_edge_xmin_ymin(c_size, c_vert, sl_rng,      &
     &          kpe, knp, ioff_gl, koff, nd, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: koff
      integer (kind = kint), intent(in) :: kpe, knp
      integer (kind = kint), intent(in) :: nd
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      integer (kind = kint) :: i, j, k
      integer (kind = kint) ::is0, js0, ks0
      integer (kind = kint) ::ie0, je0, ke0
!
       is0 = 1
       ie0 = c_size%ndepth
       js0 = 1
       je0 = c_size%ndepth
       ks0 = sl_rng%ks
       ke0 = sl_rng%ke
       if(nd.eq.3 .and. knp.gt.0) ke0 = ke0-1
       if(nd.eq.3 .and. kpe.eq.c_size%ndz .and. knp.eq.0) ke0 = ke0-1
!
       do k = ks0, ke0
        do j = js0, je0
         do i = is0, ie0

          inod = inod + 1

          loc_id%edge_id_lc(i,j,k,nd) =  inod
          node%inod_global(inod) = ioff_gl + i + (j-1) * c_size%ndepth  &
     &                + (koff+k-1) * c_size%ndepth**2

          if (nd .eq. 1) then
           node%xx(inod,1) = c_size%xmin + ( dble(i)-half )             &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymin + dble(j-1)                    &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(koff+k)
          else if (nd .eq. 2) then
           node%xx(inod,1) = c_size%xmin + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymin + ( dble(j)-half )             &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(koff+k)
          else if (nd .eq. 3) then
           node%xx(inod,1) = c_size%xmin + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymin + dble(j-1)                    &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz_edge(koff+k)
          end if
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmin_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_xmax_ymin(c_size, c_vert, sl_rng,      &
     &          kpe, knp, ioff_gl, koff, nd, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: koff
      integer (kind = kint), intent(in) :: kpe, knp
      integer (kind = kint), intent(in) :: nd
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      integer (kind = kint) :: i, j, k, i1
      integer (kind = kint) :: is0, js0, ks0
      integer (kind = kint) :: ie0, je0, ke0
!
       is0 = 1
       ie0 = c_size%ndepth
       js0 = 1
       je0 = c_size%ndepth
       ks0 = sl_rng%ks
       ke0 = sl_rng%ke
       if(nd.eq.1 ) ie0 = ie0 - 1
       if(nd.eq.3 .and. knp.gt.0) ke0 = ke0-1
       if(nd.eq.3 .and. kpe.eq.c_size%ndz .and. knp.eq.0) ke0 = ke0-1
!
       do k = ks0, ke0
        do j = js0, je0
         do i = is0, ie0

          inod = inod + 1
          i1 = c_size%nxi + c_size%ndepth + i

          loc_id%edge_id_lc(i1,j,k,nd) =  inod
          node%inod_global(inod) = ioff_gl + i + (j-1) * (ie0-is0+1)    &
     &                + (koff+k-1) * (ie0 - is0 + 1) * c_size%ndepth

          if (nd .eq. 1) then
           node%xx(inod,1) = c_size%xmax + (dble(i+c_size%ndepth)-half) &
     &                       * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymin + dble(j-1)                    &
     &                       * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(koff+k)
          else if (nd .eq. 2) then
           node%xx(inod,1) = c_size%xmax + ( dble(i+c_size%ndepth-1) )  &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymin + ( dble(j)-half )             &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(koff+k)
          else if (nd .eq. 3) then
           node%xx(inod,1) = c_size%xmax + ( dble(i+c_size%ndepth-1) )  &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymin + dble(j-1)                    &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz_edge(koff+k)
          end if
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmax_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_xmax_ymax(c_size, c_vert, sl_rng,      &
     &          kpe, knp, ioff_gl, koff, nd, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: koff
      integer (kind = kint), intent(in) :: kpe, knp
      integer (kind = kint), intent(in) :: nd
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      integer (kind = kint) :: i, j, k, i1, j1
      integer (kind = kint) :: is0, js0, ks0
      integer (kind = kint) :: ie0, je0, ke0
!
       is0 = 1
       ie0 = c_size%ndepth
       js0 = 1
       je0 = c_size%ndepth
       ks0 = sl_rng%ks
       ke0 = sl_rng%ke
       if(nd.eq.1 ) ie0 = ie0 - 1
       if(nd.eq.2 ) je0 = je0 - 1
       if(nd.eq.3 .and. knp.gt.0) ke0 = ke0-1
       if(nd.eq.3 .and. kpe.eq.c_size%ndz .and. knp.eq.0) ke0 = ke0-1
!
       do k = ks0, ke0
        do j = js0, je0
         do i = is0, ie0

          inod = inod + 1
          i1 = c_size%nxi + c_size%ndepth + i
          j1 = c_size%nyi + c_size%ndepth + j
          loc_id%edge_id_lc(i1,j1,k,nd) =  inod
          node%inod_global(inod) = ioff_gl + i + (j-1) * (ie0-is0+1)    &
     &                + (koff+k-1) * (ie0 - is0 + 1)                    &
     &                             * (je0 - js0 + 1)

          if (nd .eq. 1) then
           node%xx(inod,1) = c_size%xmax + (dble(i+c_size%ndepth)-half) &
     &               * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymax + dble(c_size%ndepth+j-1)      &
     &               * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(koff+k)
          else if (nd .eq. 2) then
           node%xx(inod,1) = c_size%xmax + dble(i+c_size%ndepth-1)      &
     &               * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymax + (dble(c_size%ndepth+j)-half) &
     &               * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(koff+k)
          else if (nd .eq. 3) then
           node%xx(inod,1) = c_size%xmax + dble(i+c_size%ndepth-1)      &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymax + dble(c_size%ndepth+j-1)      &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz_edge(koff+k)
          end if
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmax_ymax
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_xmin_ymax(c_size, c_vert, sl_rng,      &
     &          kpe, knp, ioff_gl, koff, nd, loc_id, node, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: koff
      integer (kind = kint), intent(in) :: kpe, knp
      integer (kind = kint), intent(in) :: nd
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      integer (kind = kint) :: i, j, k, j1
      integer (kind = kint) :: is0, js0, ks0
      integer (kind = kint) :: ie0, je0, ke0
!
      is0 = 1
      ie0 = c_size%ndepth
      js0 = 1
      je0 = c_size%ndepth
      ks0 = sl_rng%ks
      ke0 = sl_rng%ke
      if(nd.eq.2) je0 = je0 - 1
      if(nd.eq.3 .and. knp.gt.0) ke0 = ke0-1
      if(nd.eq.3 .and. kpe.eq.c_size%ndz .and. knp.eq.0) ke0 = ke0-1
!
      do k = ks0, ke0
        do j = js0, je0
         do i = is0, ie0

          inod = inod + 1
          j1 = c_size%nyi + c_size%ndepth + j

          loc_id%edge_id_lc(i,c_size%nyi+c_size%ndepth+j,k,nd) =  inod
          node%inod_global(inod) = ioff_gl + i + (j-1) * c_size%ndepth  &
     &                + (koff+k-1) * c_size%ndepth * (je0 - js0 + 1)

          if (nd .eq. 1) then
           node%xx(inod,1) = c_size%xmin + ( dble(i)-half )             &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymax + dble(c_size%ndepth+j-1)      &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(koff+k)
          else if (nd .eq. 2) then
           node%xx(inod,1) = c_size%xmin + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymax + (dble(c_size%ndepth+j)-half) &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz(koff+k)
          else if (nd .eq. 3) then
           node%xx(inod,1) = c_size%xmin + dble(i-1)                    &
     &                      * c_size%xsize / dble(c_size%nx_all)
           node%xx(inod,2) = c_size%ymax + dble(c_size%ndepth+j-1)      &
     &                      * c_size%ysize / dble(c_size%ny_all)
           node%xx(inod,3) = c_vert%zz_edge(koff+k)
          end if
         end do
        end do
      end do
!
      end subroutine set_sleeve_edge_xmin_ymax
!
!  ---------------------------------------------------------------------
!
      end module sleeve_edge_corner_cube
