!set_comm_edge_4_cube.f90
!     module set_comm_edge_4_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!       subroutine count_im_edge                                       &
!!      &         (sl_rng, ndz, kpe, inp, jnp, knp, inod, nd)
!!       subroutine set_im_edge(sl_rng, loc_id, ndz, kpe, inp, jnp, knp,&
!!      &          nd, ntot_import, item_import, inod)
!!        type(slleve_range), intent(in) :: sl_rng
!!
!!       subroutine count_ex_edge                                       &
!!      &        (sl_rng, ndz, kpe, inp, jnp, knp, inod, nd)
!!       subroutine set_ex_edge(sl_rng, loc_id, ndz, kpe, inp, jnp, knp,&
!!      &          nd, ntot_export, item_export, inod)
!!        type(slleve_range), intent(in) :: sl_rng
!!        type(local_node_id_cube), intent(in) :: loc_id
!
      module set_comm_edge_4_cube
!
      use m_precision
!
      use t_sleeve_cube
      use t_local_node_id_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine count_im_edge                                         &
      &         (sl_rng, ndz, kpe, inp, jnp, knp, inod, nd)
!
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ndz
      integer (kind = kint), intent(in) :: kpe, inp, jnp, knp
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(inout) :: inod
!
!
      if(nd.eq.1 .and. inp.eq. 1 ) then
        inod = inod + (sl_rng%ie - sl_rng%is    )                       &
     &               * (sl_rng%je - sl_rng%js + 1)                      &
     &               * (sl_rng%ke - sl_rng%ks + 1)
      else if(nd.eq.2 .and. jnp.eq. 1 ) then
        inod = inod + (sl_rng%ie - sl_rng%is + 1)                       &
     &               * (sl_rng%je - sl_rng%js    )                      &
     &               * (sl_rng%ke - sl_rng%ks + 1)
      else if(nd.eq.3 .and. kpe.eq.ndz .and. knp.eq. 0) then
        inod = inod + (sl_rng%ie - sl_rng%is + 1)                       &
     &               * (sl_rng%je - sl_rng%js + 1)                      &
     &               * (sl_rng%ke - sl_rng%ks  )
      else if(nd.eq.3 .and.  knp.eq. 1 ) then
        inod = inod + (sl_rng%ie - sl_rng%is + 1)                       &
     &               * (sl_rng%je - sl_rng%js + 1)                      &
     &               * (sl_rng%ke - sl_rng%ks  )
      else
        inod = inod + (sl_rng%ie - sl_rng%is + 1)                       &
     &               * (sl_rng%je - sl_rng%js + 1)                      &
     &               * (sl_rng%ke - sl_rng%ks + 1)
      end if
!
      end subroutine count_im_edge
!
! ----------------------------------------------------------------------
!
       subroutine set_im_edge(sl_rng, loc_id, ndz, kpe, inp, jnp, knp,  &
      &          nd, ntot_import, item_import, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: kpe, inp, jnp, knp
      integer (kind = kint), intent(in) :: ndz
      integer (kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ntot_import
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
      integer (kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng_1
      integer (kind = kint) :: i, j, k
!
!
      call copy_slleve_size(sl_rng, sl_rng_1)
      if(nd.eq.1 .and. inp.eq. 1) sl_rng_1%ie = sl_rng_1%ie - 1
      if(nd.eq.2 .and. jnp.eq. 1) sl_rng_1%je = sl_rng_1%je - 1
      if(nd.eq.3 .and. knp.eq. 1) sl_rng_1%ke = sl_rng_1%ke - 1
      if(nd.eq.3 .and. kpe.eq.ndz .and. knp.eq. 0)                      &
     &                               sl_rng_1%ke = sl_rng_1%ke - 1
!
      do k = sl_rng_1%ks, sl_rng_1%ke
        do j = sl_rng_1%js, sl_rng_1%je
          do i = sl_rng_1%is, sl_rng_1%ie

            inod = inod + 1
            item_import(inod) =  loc_id%edge_id_lc(i,j,k,nd)

          enddo
        enddo
      enddo
!
      end subroutine set_im_edge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_ex_edge                                          &
      &        (sl_rng, ndz, kpe, inp, jnp, knp, inod, nd)
!
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ndz
      integer (kind = kint), intent(in) :: kpe, inp, jnp, knp
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(inout) :: inod
!
!
      if(nd.eq.1 .and. inp.eq. -1) then
        inod = inod + (sl_rng%ie - sl_rng%is    )                       &
     &               * (sl_rng%je - sl_rng%js + 1)                      &
     &               * (sl_rng%ke - sl_rng%ks + 1)
      else if(nd.eq.2 .and. jnp.eq. -1) then
        inod = inod + (sl_rng%ie - sl_rng%is + 1)                       &
     &               * (sl_rng%je - sl_rng%js    )                      &
     &               * (sl_rng%ke - sl_rng%ks + 1)
      else if(nd.eq.3 .and. kpe.eq.ndz .and. knp.eq. 0) then
        inod = inod + (sl_rng%ie - sl_rng%is + 1)                       &
     &               * (sl_rng%je - sl_rng%js + 1)                      &
     &               * (sl_rng%ke - sl_rng%ks  )
      else if(nd.eq.3 .and.  knp.eq. -1) then
        inod = inod + (sl_rng%ie - sl_rng%is + 1)                       &
     &               * (sl_rng%je - sl_rng%js + 1)                      &
     &               * (sl_rng%ke - sl_rng%ks  )
      else
        inod = inod + (sl_rng%ie - sl_rng%is + 1)                       &
     &               * (sl_rng%je - sl_rng%js + 1)                      &
     &               * (sl_rng%ke - sl_rng%ks + 1)
      end if
!
      end subroutine count_ex_edge
!
! ----------------------------------------------------------------------
!
       subroutine set_ex_edge(sl_rng, loc_id, ndz, kpe, inp, jnp, knp,  &
      &          nd, ntot_export, item_export, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer(kind = kint), intent(in) :: ndz
      integer(kind = kint), intent(in) :: kpe, inp, jnp, knp
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ntot_export
!
      integer(kind = kint), intent(inout) :: item_export(ntot_export)
      integer(kind = kint), intent(inout) :: inod
!
      type(slleve_range) :: sl_rng_1
      integer (kind = kint) :: i, j, k
!
!
      call copy_slleve_size(sl_rng, sl_rng_1)
      if(nd.eq.1 .and. inp.eq. -1) sl_rng_1%ie = sl_rng_1%ie-1
      if(nd.eq.2 .and. jnp.eq. -1) sl_rng_1%je = sl_rng_1%je-1
      if(nd.eq.3 .and. knp.eq. -1) sl_rng_1%ke = sl_rng_1%ke-1
      if(nd.eq.3 .and. kpe.eq.ndz .and. knp.eq. 0)                      &
     &                                sl_rng_1%ke = sl_rng_1%ke-1
      do k = sl_rng_1%ks, sl_rng_1%ke
        do j = sl_rng_1%js, sl_rng_1%je
          do i = sl_rng_1%is, sl_rng_1%ie

            inod = inod + 1
            item_export(inod) =  loc_id%edge_id_lc(i,j,k,nd)

          enddo
        enddo
      enddo
!
      end subroutine set_ex_edge
!
! ----------------------------------------------------------------------
!
      end module set_comm_edge_4_cube
