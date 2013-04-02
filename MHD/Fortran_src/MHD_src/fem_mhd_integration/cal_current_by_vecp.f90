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
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data_MHD
      use m_finite_element_matrix
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
       call int_surf_current_diffuse
!
       call cal_multi_pass_4_vector_ff
       call cal_ff_2_vector(d_nod(1,iphys%i_current), ff, ml)
!
!    communication
!
       call vector_send_recv(iphys%i_current)
!
      end subroutine int_current_diffuse
!
! ----------------------------------------------------------------------
!
      end module cal_current_by_vecp
