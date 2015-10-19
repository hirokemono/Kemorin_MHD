!cal_current_by_vecp.f90
!      module cal_current_by_vecp
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!
!      subroutine int_current_diffuse
!
      module cal_current_by_vecp
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_current_diffuse
!
      use m_machine_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_geometry_data_MHD
      use m_finite_element_matrix
      use m_jacobian_sf_grp
      use m_int_vol_data
!
      use m_node_phys_data
      use m_node_phys_address
!
      use cal_multi_pass
      use cal_for_ffs
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
      use int_vol_current_by_vecp
      use int_surf_diffuse_terms
!
!
!  Volume integration
!
      call int_vol_current_diffuse
!
!  for boundary conditions
!
      call int_surf_current_diffuse                                     &
     &   (ele1, surf1, sf_grp1, jac1_sf_grp_2d_q)
!
      call cal_multi_pass_4_vector_ff
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_l%ff, m1_lump%ml, nod_fld1%ntot_phys,                      &
     &    iphys%i_current, nod_fld1%d_fld)
!
!    communication
!
      call vector_send_recv                                             &
     &   (iphys%i_current, node1, nod_comm, nod_fld1)
!
      end subroutine int_current_diffuse
!
! ----------------------------------------------------------------------
!
      end module cal_current_by_vecp
