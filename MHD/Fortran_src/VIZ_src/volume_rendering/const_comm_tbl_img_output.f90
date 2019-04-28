!const_comm_tbl_img_output.f90
!
!      module const_comm_tbl_img_output
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_const_comm_tbl_img_output                          &
!!     &         (irank_image_file, istack_recv_image, num_pixel_xy,    &
!!     &          irank_4_composit, item_recv_image, npixel_4_composit, &
!!     &          num_pixel_recv, img_output_tbl)
!!        type(calypso_comm_table), intent(inout) :: img_output_tbl
!!
      module const_comm_tbl_img_output
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_comm_tbl_img_output                            &
     &         (irank_image_file, istack_recv_image, num_pixel_xy,      &
     &          irank_4_composit, item_recv_image, npixel_4_composit,   &
     &          num_pixel_recv, img_output_tbl)
!
      use t_calypso_comm_table
!
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: istack_recv_image(0:nprocs)
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in)                                  &
     &                     :: irank_4_composit(num_pixel_xy)
      integer(kind = kint), intent(in) :: item_recv_image(num_pixel_xy)
      integer(kind = kint), intent(in) :: npixel_4_composit
!
      integer(kind = kint), intent(inout) :: num_pixel_recv
      type(calypso_comm_table), intent(inout) :: img_output_tbl
!
!
      call count_export_pe_pvr_output                                   &
     &   (npixel_4_composit, img_output_tbl%nrank_export)
!
      call alloc_calypso_export_num(img_output_tbl)
      call count_export_item_pvr_output                                 &
     &   (irank_image_file, npixel_4_composit,                          &
     &    img_output_tbl%nrank_export, img_output_tbl%ntot_export,      &
     &    img_output_tbl%irank_export, img_output_tbl%istack_export)
!
      call alloc_calypso_export_item(img_output_tbl)
      call set_export_item_pvr_output                                   &
     &   (img_output_tbl%ntot_export, img_output_tbl%item_export)
!
!
      call count_import_pe_pvr_output                                   &
     &   (irank_image_file, istack_recv_image,                          &
     &    img_output_tbl%nrank_import)
!
      call alloc_calypso_import_num(img_output_tbl)
      call count_import_item_pvr_output                                 &
     &   (irank_image_file, istack_recv_image,                          &
     &    num_pixel_xy, irank_4_composit, item_recv_image,              &
     &    img_output_tbl%nrank_import, img_output_tbl%ntot_import,      &
     &    img_output_tbl%irank_import, img_output_tbl%istack_import,    &
     &    img_output_tbl%iflag_self_copy, num_pixel_recv)
!
      call alloc_calypso_import_item(num_pixel_recv, img_output_tbl)
      call set_import_item_pvr_output                                   &
     &    (num_pixel_xy, item_recv_image,                               &
     &     img_output_tbl%ntot_import, num_pixel_recv,                  &
     &     img_output_tbl%item_import, img_output_tbl%irev_import)
!
      end subroutine s_const_comm_tbl_img_output
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_export_pe_pvr_output                             &
     &         (npixel_4_composit, ncomm_send_pixel_output)
!
      integer(kind = kint), intent(in) :: npixel_4_composit
      integer(kind = kint), intent(inout) :: ncomm_send_pixel_output
!
!
      ncomm_send_pixel_output = 0
      if(npixel_4_composit .gt. 0) ncomm_send_pixel_output = 1
!
      end subroutine count_export_pe_pvr_output
!
!  ---------------------------------------------------------------------
!
      subroutine count_export_item_pvr_output                           &
     &         (irank_image_file, npixel_4_composit,                    &
     &          ncomm_send_pixel_output, ntot_send_pixel_output,        &
     &          irank_send_pixel_output, istack_send_pixel_output)
!
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: npixel_4_composit
!
      integer(kind = kint), intent(in) :: ncomm_send_pixel_output
      integer(kind = kint), intent(inout) :: ntot_send_pixel_output
      integer(kind = kint), intent(inout)                               &
     &          :: irank_send_pixel_output(ncomm_send_pixel_output)
      integer(kind = kint), intent(inout)                               &
     &          :: istack_send_pixel_output(0:ncomm_send_pixel_output)
!
!
      istack_send_pixel_output(0) = 0
      if(ncomm_send_pixel_output .eq. 1) then
        irank_send_pixel_output(ncomm_send_pixel_output)                &
     &           = irank_image_file
        istack_send_pixel_output(ncomm_send_pixel_output)               &
     &           = npixel_4_composit
      end if
      ntot_send_pixel_output                                            &
     &           = istack_send_pixel_output(ncomm_send_pixel_output)
!
      end subroutine count_export_item_pvr_output
!
!  ---------------------------------------------------------------------
!
      subroutine set_export_item_pvr_output                             &
     &         (ntot_send_pixel_output, item_send_pixel_output)
!
      integer(kind = kint), intent(in) :: ntot_send_pixel_output
      integer(kind = kint), intent(inout)                               &
     &              :: item_send_pixel_output(ntot_send_pixel_output)
!
      integer(kind = kint) :: inum
!
!
!$omp parallel do
      do inum = 1, ntot_send_pixel_output
        item_send_pixel_output(inum) = inum
      end do
!$omp end parallel do
!
      end subroutine set_export_item_pvr_output
!
!  ---------------------------------------------------------------------
!
      subroutine count_import_pe_pvr_output                             &
     &         (irank_image_file, istack_recv_image,                    &
     &          ncomm_recv_pixel_output)
!
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: istack_recv_image(0:nprocs)
!
      integer(kind = kint), intent(inout) :: ncomm_recv_pixel_output
!
      integer(kind = kint) :: ip, num
!
!
      ncomm_recv_pixel_output = 0
      if(my_rank .eq. irank_image_file) then
        do ip = 1, nprocs
          num = istack_recv_image(ip) - istack_recv_image(ip-1)
          if(num .gt. 0) then
            ncomm_recv_pixel_output = ncomm_recv_pixel_output + 1
          end if
        end do
      end if
!
      end subroutine count_import_pe_pvr_output
!
!  ---------------------------------------------------------------------
!
      subroutine count_import_item_pvr_output                           &
     &         (irank_image_file, istack_recv_image,                    &
     &          num_pixel_xy, irank_4_composit, item_recv_image,        &
     &          ncomm_recv_pixel_output, ntot_recv_pixel_output,        &
     &          irank_recv_pixel_output, istack_recv_pixel_output,      &
     &          iself_pixel_output, num_pixel_recv)
!
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: istack_recv_image(0:nprocs)
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: irank_4_composit(num_pixel_xy)
      integer(kind = kint), intent(in) :: item_recv_image(num_pixel_xy)
!
      integer(kind = kint), intent(in) :: ncomm_recv_pixel_output
!
      integer(kind = kint), intent(inout) :: num_pixel_recv
      integer(kind = kint), intent(inout) :: iself_pixel_output
      integer(kind = kint), intent(inout) :: ntot_recv_pixel_output
      integer(kind = kint), intent(inout)                               &
     &          :: irank_recv_pixel_output(ncomm_recv_pixel_output)
      integer(kind = kint), intent(inout)                               &
     &          :: istack_recv_pixel_output(0:ncomm_recv_pixel_output)
!
      integer(kind = kint) :: ip, icou, ist, num, ipix
!
      iself_pixel_output = 0
      num_pixel_recv = 0
      istack_recv_pixel_output(0) = 0
      if(my_rank .eq. irank_image_file) then
        num_pixel_recv = num_pixel_xy
!
        icou = 0
        do ip = 1, nprocs
          ist = istack_recv_image(ip-1)
          num = istack_recv_image(ip) - istack_recv_image(ip-1)
          ipix = item_recv_image(ist+1)
!
          if(num .gt. 0) then
            icou = icou + 1
            write(*,*) 'icou', icou, ncomm_recv_pixel_output
            istack_recv_pixel_output(icou) = istack_recv_image(ip)
            irank_recv_pixel_output(icou) = irank_4_composit(ipix)
          end if
          if(irank_4_composit(ipix) .eq. irank_image_file) then
            iself_pixel_output = 1
          end if
        end do
      end if
      ntot_recv_pixel_output                                            &
     &      = istack_recv_pixel_output(ncomm_recv_pixel_output)
!
      end subroutine count_import_item_pvr_output
!
!  ---------------------------------------------------------------------
!
      subroutine set_import_item_pvr_output                             &
     &         (num_pixel_xy, item_recv_image,                          &
     &          ntot_recv_pixel_output, num_pixel_recv,                 &
     &          item_recv_pixel_output, irev_recv_pixel_output)
!
      integer(kind = kint), intent(in) :: ntot_recv_pixel_output
      integer(kind = kint), intent(in) :: num_pixel_xy, num_pixel_recv
      integer(kind = kint), intent(in) :: item_recv_image(num_pixel_xy)
!
      integer(kind = kint), intent(inout)                               &
     &              :: item_recv_pixel_output(ntot_recv_pixel_output)
      integer(kind = kint), intent(inout)                               &
     &              :: irev_recv_pixel_output(num_pixel_recv)
!
      integer(kind = kint) :: inum, ipix
!
!
      if(num_pixel_recv .le. 0) return
!
!$omp parallel workshare
      irev_recv_pixel_output(1:num_pixel_recv) = 0
!$omp end parallel workshare
!
      do inum = 1, ntot_recv_pixel_output
        ipix = item_recv_image(inum)
        item_recv_pixel_output(inum) = ipix
        irev_recv_pixel_output(ipix) = inum
      end do
!
      end subroutine set_import_item_pvr_output
!
!  ---------------------------------------------------------------------
!
      end module const_comm_tbl_img_output
