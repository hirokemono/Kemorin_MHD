!
!      module copy_w_filters_4_IO
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine copy_w_filter_stacks_from_IO
!      subroutine copy_w_filter_weights_from_IO
!
!      subroutine copy_w_filter_geometry_from_IO
!      subroutine copy_w_filter_comm_tbl_from_IO
!
      module copy_w_filters_4_IO
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_w_filter_stacks_from_IO
!
      use m_3d_w_filter_coef
      use m_combained_filter_IO
!
!
      ngrp_nod_3d_w_fil = ngrp_nod_filter_IO
      call allocate_num_w_filtering_comb
!
      grp_name_3d_w_fil(1:ngrp_nod_3d_w_fil)                            &
     &         = grp_name_filter_IO(1:ngrp_nod_3d_w_fil)
      num_nod_3d_w_fil(1:ngrp_nod_3d_w_fil)                             &
     &         = num_nod_filter_IO(1:ngrp_nod_3d_w_fil)
      istack_nod_3d_w_fil(0:ngrp_nod_3d_w_fil)                          &
     &         = istack_nod_filter_IO(0:ngrp_nod_3d_w_fil)
!
      ntot_nod_3d_w_fil = ntot_nod_filter_IO
      call allocate_inod_w_fil_comb
!
      inod_3d_w_filter(1:ntot_nod_3d_w_fil)                             &
     &      = inod_filter_IO(1:ntot_nod_3d_w_fil)
      num_near_nod_3d_w_fil(1:ntot_nod_3d_w_fil)                        &
     &      = num_near_nod_filter_IO(1:ntot_nod_3d_w_fil)
      istack_near_nod_3d_w_fil(0:ntot_nod_3d_w_fil)                     &
     &      = istack_near_nod_filter_IO(0:ntot_nod_3d_w_fil)
      ntot_near_nod_3d_w_fil = ntot_near_nod_filter_IO
!
      call deallocate_inod_filter_comb_IO
      call deallocate_num_filtering_IO
!
      end subroutine copy_w_filter_stacks_from_IO
!
!  ---------------------------------------------------------------------
!
      subroutine copy_w_filter_weights_from_IO
!
      use m_3d_w_filter_coef
      use m_combained_filter_IO
!
!
      ntot_near_nod_3d_w_fil = ntot_near_nod_filter_IO
      call allocate_3d_w_fil_comb
!
      inod_near_nod_3d_w(1:ntot_near_nod_3d_w_fil)                      &
     &      = inod_near_nod_IO(1:ntot_near_nod_3d_w_fil)
      filter_weight_3d_w(1:ntot_near_nod_3d_w_fil)                      &
     &      = filter_weight_IO(1:ntot_near_nod_3d_w_fil)
!
      call deallocate_3d_filter_data_IO
!
      end subroutine copy_w_filter_weights_from_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_w_filter_geometry_from_IO
!
      use m_read_mesh_data
      use m_nod_w_filter_comm_table
!
      nnod_w_filtering = numnod_dummy
      inter_nod_w_filter = internal_node_dummy
      call allocate_globalnod_w_fil
!
      id_globalnod_w_fil(1:nnod_w_filtering)                            &
     &      = globalnodid_dummy(1:nnod_w_filtering)
      xx_w_filter(1:nnod_w_filtering,1)                                 &
     &      = xx_dummy(1:nnod_w_filtering,1)
      xx_w_filter(1:nnod_w_filtering,2)                                 &
     &      = xx_dummy(1:nnod_w_filtering,2)
      xx_w_filter(1:nnod_w_filtering,3)                                 &
     &      = xx_dummy(1:nnod_w_filtering,3)
!
      call deallocate_node_data_dummy
!
      end subroutine copy_w_filter_geometry_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_w_filter_comm_tbl_from_IO
!
      use m_nod_w_filter_comm_table
      use m_comm_data_IO
      use copy_communication_table
!
      integer(kind = kint) :: i
!
!
      num_neib_w_fil = num_neib_domain_IO
!
      call allocate_neib_w_fil_id
      call allocate_w_fil_import_num
      call allocate_w_fil_export_num
!
      call copy_num_communication(num_neib_w_fil, id_neib_w_fil,        &
     &    istack_import_w_fil, istack_export_w_fil,                     &
     &    ntot_import_w_fil, ntot_export_w_fil,                         &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO)
      call copy_num_import_export(num_neib_w_fil,                       &
     &    num_import_w_fil, num_export_w_fil,                           &
     &    istack_import_w_fil, istack_export_w_fil)
!
      call allocate_w_fil_import_item
      call allocate_w_fil_export_item
!
      call copy_communication_item(ntot_import_w_fil,                   &
     &    ntot_export_w_fil, item_import_w_fil, item_export_w_fil,      &
     &    item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_w_filter_comm_tbl_from_IO
!
!-----------------------------------------------------------------------
!
      end module copy_w_filters_4_IO
